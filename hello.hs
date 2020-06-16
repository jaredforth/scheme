 module Main where
 import System.Environment
 
 main :: IO ()
 main = do
     args <- getArgs
     let arg1 = args !! 0
     let arg2 = args !! 1
     let a = read arg1 :: Integer
     let b = read arg2 :: Integer
     let resultI = a + b
     let result = show resultI
     putStrLn ("Result: " ++ result)
    --  let resStr = show result
    --  putStrLn ("The result is: " ++ resStr)