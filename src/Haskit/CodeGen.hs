{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Haskit.CodeGen(CodegenState(..), make) where
import           Control.Monad.State.Strict
import           Data.DList                 (DList)
import qualified Data.DList                 as L
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.Word
import           Haskit.Parser
import           LuaJIT.ByteCode

type LocalIdx = Word8
type ConstIdx = Word8

data CodegenState =
    CodegenState {
        symbols   :: HashMap String Var,
        constants :: DList KNum,
        opcodes   :: DList Opcode,
        localIdx  :: LocalIdx
    } deriving (Show)

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
    deriving (Functor, Applicative, Monad, MonadState CodegenState)


data Var = Local LocalIdx | Const ConstIdx
    deriving (Show)

getVar :: String -> Codegen Var
getVar nm = do
    syms <- gets symbols
    case Map.lookup nm syms of
      Just x  -> return x
      Nothing -> error $ "Unknown binding: " ++ show nm

bind :: String -> Var -> Codegen ()
bind nm var = modify $ \s -> s { symbols = Map.insert nm var (symbols s) }

constant :: KNum -> Codegen ConstIdx
constant num = do
    c <- gets constants
    let idx = length c
    modify $ \s -> s { constants = L.snoc c num }
    return $ fromIntegral idx

pushCode :: DList Opcode -> Codegen ()
pushCode ops = modify $ \s -> s { opcodes = L.append (opcodes s) ops }

local :: Codegen LocalIdx
local = do
    idx <- gets localIdx
    modify $ \s -> s { localIdx = idx + 1 }
    return idx

build :: AExpr -> Codegen LocalIdx
build (ABinary Add (IntConst a) (IntConst b)) = do
    ca <- constant . KI64 $ fromIntegral a
    cb <- constant . KI64 $ fromIntegral b
    la <- local
    res <- local
    pushCode $ L.fromList
        [ knum la (fromIntegral ca)
        , addnv res la cb
        ]
    return res

make :: AExpr -> CodegenState
make expr = execState (runCodegen $ build expr) $
    CodegenState { symbols = Map.empty
                 , constants = L.empty
                 , opcodes = L.empty
                 , localIdx = 0
                 }
