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
runhaskell -package-conf=.cabal-sandbox/x86_64-osx-ghc-7.8.3-packages.conf.d -isrc -itest test/Raskell/Parser/RubyGrammarSpec.hs
```

# TODO

* parser
* all the rest!
* read [https://github.com/JakeWheat/intro_to_parsing](intro to
  parsing)
* read [http://hspec.github.io/](hspec)
