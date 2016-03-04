module Main where

import Lib

main :: IO ()
main = do
  putStrLn "Enter your name:"
  s <- getLine
  putStrLn (someFunc s)
