[![Build Status](https://travis-ci.com/jaredforth/scheme.svg?token=mH2pScYxqRkBEzpBQAu6&branch=master)](https://travis-ci.com/jaredforth/scheme)
![GitHub](https://img.shields.io/github/license/jaredforth/scheme)

# Scheme 

A toy implementation of a subset of Scheme in Haskell.

## Compilation 

To compile, the `compile.sh` is added for convenience. On the command line, run `./compile.sh` to create an executable. This file simply runs the [GHC compiler](https://www.haskell.org/ghc/) with a couple flags passed:

```shell script
ghc -package parsec -fglasgow-exts -o scheme --make scheme.hs
```

## Running the Executable

On the command line, run `./scheme` to start the REPL. To load the standard library, run `(load "stdlib.scm")` in the REPL.

## TODOs

- Document with [Haddock](https://haskell-haddock.readthedocs.io/en/latest/markup.html)
- Break into seperate files
- Compile with [Cabal](https://www.haskell.org/cabal/)
