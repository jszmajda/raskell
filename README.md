# Raskell

Lots to do!

# Getting Started

```
cabal install --enable-tests
cabal test
```

## Running an individual test

From the root of the repo:

```
runhaskell -package-db=.cabal-sandbox/x86_64-osx-ghc-7.8.3-packages.conf.d -isrc -itest Spec
```

# TODO

* parser
* all the rest!
* look into [Alex](https://www.haskell.org/alex/)
    * [Lua parser with Alex](https://github.com/osa1/language-lua/blob/master/src/Text/Parsec/LTok.hs)
* look into [happy](https://www.haskell.org/happy/doc/html/index.html)
* read [intro to parsing](https://github.com/JakeWheat/intro_to_parsing)
* read [hspec](http://hspec.github.io/)
