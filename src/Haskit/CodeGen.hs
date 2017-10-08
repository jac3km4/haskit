{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Haskit.CodeGen(CodegenState(..), make) where
import           Control.Monad.State.Strict
import           Data.DList                 (DList)
import qualified Data.DList                 as L
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.Word
import qualified Haskit.Parser              as P
import           LuaJIT.ByteCode

type LocalIdx = Word8
type ConstIdx = Word8

data CodegenState =
    CodegenState {
        symbols   :: HashMap String Var,
        constants :: DList KNum,
        opcodes   :: DList Instruction,
        localIdx  :: LocalIdx
    } deriving (Show)

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
    deriving (Functor, Applicative, Monad, MonadState CodegenState)

data Var = Local LocalIdx | Const ConstIdx
    deriving (Show)

bind :: String -> Var -> Codegen ()
bind nm var = modify $ \s -> s { symbols = Map.insert nm var (symbols s) }

getSym :: String -> Codegen Var
getSym nm = do
    syms <- gets symbols
    case Map.lookup nm syms of
      Just x  -> return x
      Nothing -> error $ "Unknown binding: " ++ show nm

constant :: KNum -> Codegen ConstIdx
constant num = do
    c <- gets constants
    let idx = length c
    modify $ \s -> s { constants = L.snoc c num }
    return $ fromIntegral idx

local :: Codegen LocalIdx
local = do
    idx <- gets localIdx
    modify $ \s -> s { localIdx = idx + 1 }
    return idx

instr :: Instruction -> Codegen ()
instr op = modify $ \s -> s { opcodes = L.snoc (opcodes s) op }

binop :: P.BinOp -> Var -> Var -> Codegen Var
binop op (Const a) (Const b) = do
    l <- local
    instr . knum l $ fromIntegral a
    binop op (Local l) (Const b)
binop op (Const a) (Local b) = do
    binop op (Local b) (Const a)
binop op (Local a) (Const b) =
    local >>= \l -> Local l <$ instr (opvn op l a b)
    where
        opvn P.Add  = addvn
        opvn P.Sub  = subvn
        opvn P.Mult = mulvn
        opvn P.Div  = divvn
binop op (Local a) (Local b) = do
    local >>= \l -> Local l <$ instr (opvv op l a b)
    where
        opvv P.Add  = addvv
        opvv P.Sub  = subvv
        opvv P.Mult = mulvv
        opvv P.Div  = divvv

unop :: P.UnOp -> Var -> Codegen Var
unop op (Const c) = do
    l <- local
    instr . knum l $ fromIntegral c
    unop op $ Local l
unop op (Local c) =
    local >>= \l ->
        Local l <$ instr (opv op l $ fromIntegral c)
    where
        opv P.Not = bnot
        opv P.Neg = unm

build :: P.Expr -> Codegen Var
build (P.Constant (P.Ident ident)) = getSym ident
build (P.Constant (P.IntConst v)) =
    fmap Const $ constant . KI64 $ fromIntegral v
build (P.Constant (P.BoolConst v)) =
    fmap Const . constant . KI64 $ if v then 1 else 0
build (P.Binary op a b) = do
    va <- build a
    vb <- build b
    binop op va vb
build (P.Unary op c) = build c >>= unop op

make :: P.Expr -> CodegenState
make expr = execState (runCodegen $ build expr) $
    CodegenState { symbols = Map.empty
                 , constants = L.empty
                 , opcodes = L.empty
                 , localIdx = 0
                 }
