# haskit
Haskell library for evaluating expressions using the LuaJIT compiler.

The main goal is to implement a parser for a small subset of Haskell and compile it to
LuaJIT bytecode in order to make use of runtime JIT optimizations. Parsing and generating
bytecode can be done at compile-time (so that it'll be possible to do some sort of
type-checking of parameters to the LuaJIT code calls).

**Right now, this library is nowhere near useable.**
