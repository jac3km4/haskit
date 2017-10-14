{-# LANGUAGE QuasiQuotes #-}
module Lib
  ( testRun
  ) where
import qualified Foreign.Lua    as Lua
import           Haskit.Quoting

testRun :: IO ()
testRun = print =<< Lua.runLua exec

exec :: Lua.Lua Lua.LuaInteger
exec = do
  _ <- [def|fun x = let y = x + 1000 in x - y|]
  Lua.call 0 0
  Lua.callFunc "fun" $ Lua.LuaInteger 100
