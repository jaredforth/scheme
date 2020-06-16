 module Main where
 import System.Environment
 
 main :: IO ()
 main = do
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")  