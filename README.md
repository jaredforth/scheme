![Haskell CI](https://github.com/jaredforth/scheme/workflows/Haskell%20CI/badge.svg)
[![DeepScan grade](https://deepscan.io/api/teams/11311/projects/14222/branches/258612/badge/grade.svg)](https://deepscan.io/dashboard#view=project&tid=11311&pid=14222&bid=258612)
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
