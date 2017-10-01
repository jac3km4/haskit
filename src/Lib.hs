{-# LANGUAGE QuasiQuotes #-}
module Lib
    ( testRun
    ) where
import qualified Data.DList      as L
import qualified Foreign.Lua     as Lua
import           Haskit.CodeGen
import           Haskit.Parser
import           Haskit.Quoting
import           LuaJIT.ByteCode
import           Text.Megaparsec

testRun :: IO ()
testRun = do
    let cg = make [expr|1 + 3|]
    let bc = compile [ Chunk "fn" (frame cg) ]
    print bc
    res <- Lua.runLua $ luafun bc
    print res
    where
      frame cg =
        Frame { frameFlags = 0
              , paramCount = 0
              , frameSize = localIdx cg
              , uvCount = 0
              , knums = L.toList $ constants cg
              , bytecode = L.toList . L.snoc (opcodes cg) $ ret1 1 2
              , kgcs = []
              }

luafun :: ByteCode -> Lua.Lua Lua.LuaInteger
luafun bs = do
    loadByteCode bs
    Lua.call 0 0
    Lua.getglobal "fn"
    Lua.call 0 1
    Lua.peek =<< Lua.gettop
