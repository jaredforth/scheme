[![Build Status](https://travis-ci.com/jaredforth/scheme.svg?token=mH2pScYxqRkBEzpBQAu6&branch=master)](https://travis-ci.com/jaredforth/scheme)
![GitHub](https://img.shields.io/github/license/jaredforth/scheme)

# Scheme 

A toy implementation of a subset of Scheme in Haskell.

## Compilation 

This repository uses [Cabal](https://www.haskell.org/cabal/) to manage dependencies and to compile. To install dependencies and build the program, run:

```shell script
cabal install
cabal build
```

## Running the Executable

On the command line, run `./scheme` to start the REPL. This file is in `dist/build/scheme/scheme` To load the standard library, run `(load "stdlib.scm")` in the REPL.

## Building the Documentation

To document with [Haddock](https://haskell-haddock.readthedocs.io/en/latest/markup.html), run:

```shell script
haddock Main.hs --html -o docs
```

## TODOs

- Break into separate files