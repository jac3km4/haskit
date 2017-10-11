{-# LANGUAGE QuasiQuotes #-}
module Lib
    ( testRun
    ) where
import qualified Foreign.Lua     as Lua
import           Haskit.CodeGen
import           Haskit.Parser
-- import           Haskit.Quoting
import           LuaJIT.ByteCode
import           Text.Megaparsec

testRun :: IO ()
testRun = do
    let m = runParser global "test" "a x = 10000000 + x\nb = let result = a(2) * 100 in result"
    case m of
        Right cg -> do
            print cg
            let bc = compile cg
            print bc
            res <- Lua.runLua $ exec bc
            print res
        Left err -> print err

exec :: ByteCode -> Lua.Lua Lua.LuaInteger
exec bs = do
    loadByteCode bs
    Lua.call 0 0
    Lua.getglobal "b"
    Lua.call 0 1
    Lua.peek =<< Lua.gettop
