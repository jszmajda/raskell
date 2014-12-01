# Raskell

Lots to do!

# Getting Started

```
cabal install
cabal test
```

## Running an individual test

From the root of the repo:

```
runhaskell -package-db=.cabal-sandbox/x86_64-osx-ghc-7.8.3-packages.conf.d -isrc -itest test/Raskell/Parser/RubyGrammarSpec.hs
```

# TODO

* parser
* all the rest!
* look into [https://www.haskell.org/alex/](Alex)
    * [https://github.com/osa1/language-lua/blob/master/src/Text/Parsec/LTok.hs](Lua parser with Alex)
* look into [https://www.haskell.org/happy/doc/html/index.html](happy)
* read [https://github.com/JakeWheat/intro_to_parsing](intro to
  parsing)
* read [http://hspec.github.io/](hspec)
