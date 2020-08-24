{-|
Module      : Scheme.REPL
Description : Contains logic for the read-evaluate-print loop
License     : GPL-3
Maintainer  : jaredforthdev@gmail.com
Stability   : experimental
-}

module Scheme.REPL where

import System.Environment
import Control.Monad
import Control.Monad.Except

import System.IO

import Data.IORef

import Scheme.Parser
import Scheme.Primitives

-- | Immediately flush the stream
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- | Print a prompt and read a line of input
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- | Evaluate string and trap errors
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

-- | Evaluate string and print result
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action

-- | Read input, call function, and print output in an infinite loop
runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
        >>= hPutStrLn stderr

-- | Run the read-evaluate-print loop
runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint