# haskit
Haskell library for evaluating expressions using the LuaJIT compiler.

The main goal of this project is to implement a parser for a small subset of Haskell and compile it to
LuaJIT bytecode in order to make use of runtime JIT optimizations. The expressions to be compiled can
be embedded within regular Haskell code using [Quasiquotations](https://wiki.haskell.org/Quasiquotation).
Parsing and code generation is done at compile-time so that the resulting executable contains the raw bytecode
that can be passed to LuaJIT directly.

*This library is in a very early phase and many features remain unimplemented.*

# Usage
```haskell
import qualified Foreign.Lua    as Lua
import           Haskit.Quoting

testRun :: IO ()
testRun = print =<< Lua.runLua exec

exec :: Lua.Lua Lua.LuaInteger
exec = do
  _ <- [def|fun x = let y = x + 1000 in x - y|]
  Lua.call 0 0
  Lua.callFunc "fun" $ Lua.LuaInteger 100
```
