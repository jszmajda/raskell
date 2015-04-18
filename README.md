[![Stories in Ready](https://badge.waffle.io/joshsz/raskell.png?label=ready&title=Ready)](https://waffle.io/joshsz/raskell)
# Raskell

Lots to do!, Waffling about [Waffle](https://waffle.io/joshsz/raskell)

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

## Getting a useful ghci

```
ghci -package-db=.cabal-sandbox/x86_64-osx-ghc-7.8.4-packages.conf.d -isrc
```

# TODO

* parser
* all the rest!
* look into [Alex](https://www.haskell.org/alex/)
    * [Lua parser with Alex](https://github.com/osa1/language-lua/blob/master/src/Text/Parsec/LTok.hs)
* look into [happy](https://www.haskell.org/happy/doc/html/index.html)
* read [intro to parsing](https://github.com/JakeWheat/intro_to_parsing)
* read [hspec](http://hspec.github.io/)
