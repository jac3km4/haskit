{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Haskit.CodeGen
  ( compile
  ) where
import           Control.Arrow              (second)
import           Control.Monad              (forM_)
import           Control.Monad.State.Strict
import           Data.DList                 (DList)
import qualified Data.DList                 as L
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.Word
import qualified Haskit.Parser              as P
import qualified LuaJIT.ByteCode            as BC

type LocalIdx = Word8

type ConstIdx = Word8

data CodeGenState = CodeGenState
  { symbols      :: HashMap String Var
  , knums        :: DList BC.KNum
  , kgcs         :: DList BC.KGC
  , instructions :: DList BC.Instruction
  , localIdx     :: LocalIdx
  } deriving (Show)

newtype CodeGen a = CodeGen
  { runCodeGen :: State CodeGenState a
  } deriving (Functor, Applicative, Monad, MonadState CodeGenState)

data Var
  = Local LocalIdx
  | Const ConstIdx
  | Global ConstIdx
  deriving (Show)

bind :: String -> Var -> CodeGen ()
bind nm var = modify $ \s -> s {symbols = Map.insert nm var (symbols s)}

getSym :: String -> CodeGen Var
getSym nm = do
  syms <- gets symbols
  case Map.lookup nm syms of
    Just x  -> return x
    Nothing -> error $ "Unknown binding: " ++ show nm

knum :: BC.KNum -> CodeGen ConstIdx
knum num = do
  c <- gets knums
  let idx = length c
  modify $ \s -> s {knums = L.snoc c num}
  return $ fromIntegral idx

kgc :: BC.KGC -> CodeGen ConstIdx
kgc gc = do
  c <- gets kgcs
  let idx = length c
  modify $ \s -> s {kgcs = L.snoc c gc}
  return $ fromIntegral idx

local :: CodeGen LocalIdx
local = do
  idx <- gets localIdx
  modify $ \s -> s {localIdx = idx + 1}
  return idx

instr :: BC.Instruction -> CodeGen ()
instr op = modify $ \s -> s {instructions = L.snoc (instructions s) op}

binop :: P.BinOp -> Var -> Var -> CodeGen Var
binop op (Const a) (Const b) = do
  l <- local
  instr . BC.knum l $ fromIntegral a
  binop op (Local l) (Const b)
binop op (Const a) (Local b) = do
  local >>= \l -> Local l <$ instr (opnv op l a b)
  where
    opnv P.Add  = BC.addnv
    opnv P.Sub  = BC.subnv
    opnv P.Mult = BC.mulnv
    opnv P.Div  = BC.divnv
binop op (Local a) (Const b) = local >>= \l -> Local l <$ instr (opvn op l a b)
  where
    opvn P.Add  = BC.addvn
    opvn P.Sub  = BC.subvn
    opvn P.Mult = BC.mulvn
    opvn P.Div  = BC.divvn
binop op (Local a) (Local b) = do
  local >>= \l -> Local l <$ instr (opvv op l a b)
  where
    opvv P.Add  = BC.addvv
    opvv P.Sub  = BC.subvv
    opvv P.Mult = BC.mulvv
    opvv P.Div  = BC.divvv

unop :: P.UnOp -> Var -> CodeGen Var
unop op (Const c) = do
  l <- local
  instr . BC.knum l $ fromIntegral c
  unop op $ Local l
unop op (Local c) = local >>= \l -> Local l <$ instr (opv op l $ fromIntegral c)
  where
    opv P.Not = BC.bnot
    opv P.Neg = BC.unm

expr :: P.Expr -> CodeGen Var
expr (P.Constant (P.Ident ident)) = getSym ident
expr (P.Constant (P.IntConst v)) = fmap Const . knum . BC.KI64 $ fromIntegral v
expr (P.Constant (P.BoolConst v)) =
  fmap Const . knum . BC.KI64 $
  if v
    then 1
    else 0
expr (P.Binary op a b) = do
  va <- expr a
  vb <- expr b
  binop op va vb
expr (P.Unary op c) = expr c >>= unop op
expr (P.Binding sym val body) = do
  v <- expr val
  bind sym v
  expr body
expr (P.Call nm args) = do
  fl <- local
  k <- kgc $ BC.Str nm
  instr . BC.gget fl $ fromIntegral k
  forM_ args $ \arg -> do
    l <- local
    v <- expr arg
    case v of
      Local l' -> instr . BC.mov l $ fromIntegral l'
      Const c  -> instr . BC.knum l $ fromIntegral c
  instr . BC.call fl 2 $ fromIntegral (length args + 1)
  return $ Local fl

definition :: P.Expr -> CodeGen ()
definition e =
  expr e >>= \case
    Local l -> instr $ BC.ret1 l 2
    Const c -> do
      l <- local
      instr . BC.knum l $ fromIntegral c
      instr $ BC.ret1 l 2

setup :: [P.Definition] -> CodeGen ()
setup defs = do
  l <- local
  forM_ (reverse defs) $ \def -> do
    nm <- kgc . BC.Str $ P.definitionName def
    child <- kgc BC.Child
    instr . BC.fnew l $ fromIntegral nm
    instr . BC.gset l $ fromIntegral child
  instr $ BC.ret0 0 1

compile :: [P.Definition] -> BC.ByteCode
compile defs =
  BC.ByteCode $
  fmap make defs ++
  [frame 0 3 $ execState (runCodeGen $ setup defs) (initial [])]
  where
    make def =
      frame (fromIntegral . length $ P.definitionParams def) 0 .
      execState (runCodeGen . definition $ P.definitionBody def) . initial $
      P.definitionParams def
    globals =
      let names = fmap P.definitionName defs
      in fmap (second Global) $ zip names [0 ..]
    initial params =
      CodeGenState
      { symbols = Map.fromList $ globals ++ zip params (fmap Local [0 ..])
      , knums = L.empty
      , kgcs = L.empty
      , instructions = L.empty
      , localIdx = fromIntegral $ length params
      }
    frame paramCount flags st =
      BC.Frame
      { BC.frameFlags = flags
      , BC.paramCount = paramCount
      , BC.frameSize = localIdx st
      , BC.uvCount = 0
      , BC.knums = L.toList $ knums st
      , BC.kgcs = L.toList $ kgcs st
      , BC.bytecode = L.toList $ instructions st
      }
