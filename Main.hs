{-|
Module      : scheme
Description : A toy implementation of a subset of Scheme in Haskell
License     : GPL-3
Maintainer  : jaredforthdev@gmail.com
Stability   : experimental
-}

module Main where

import System.IO
import System.Environment

import Scheme.Parser
import Scheme.Primitives
import Scheme.REPL

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args
