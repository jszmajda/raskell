[![Build Status](https://travis-ci.org/jszmajda/raskell.svg)](https://travis-ci.org/jszmajda/raskell)

# Raskell

A Ruby VM in Haskell

Current status: doesn't do much, but it'll parse some basic ruby :D

Lots to do!

# Getting Started

```
stack build
stack test
```

## Running an individual test

This doesn't work anymore with stack yet.. need to look into it again
From the root of the repo:

```
runhaskell -package-db=.cabal-sandbox/x86_64-osx-ghc-7.8.3-packages.conf.d -isrc -itest Spec
```

## Getting a useful ghci

```
stack ghci
```

# TODO

* convert `[AST.Expr]` in `ProgramState` modeling to an actual tree with a root node

* more parser
* look into [Alex](https://www.haskell.org/alex/)
    * [Lua parser with Alex](https://github.com/osa1/language-lua/blob/master/src/Text/Parsec/LTok.hs)
* look into [happy](https://www.haskell.org/happy/doc/html/index.html)
* read [intro to parsing](https://github.com/JakeWheat/intro_to_parsing)
* read [hspec](http://hspec.github.io/)
