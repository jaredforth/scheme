# Scheme 

A toy implementation of a subset of Scheme in Haskell.

## Compilation 

To compile, the `compile.sh` is added for convenience. On the command line, run `./compile.sh` to create an executable. This file simply runs the [GHC compiler](https://www.haskell.org/ghc/) with a couple flags passed:

```shell script
ghc -package parsec -fglasgow-exts -o scheme --make scheme.hs
```

## Running the Executable

On the command line, run `./scheme` to start the REPL. To load the standard library, run `(load "stdlib.scm")` in the REPL.