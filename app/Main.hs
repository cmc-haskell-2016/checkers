module Main where

import Lib
import Types
import World_processing
import Graphics_processing

main :: IO ()
main = game_start

---- old main
-- main = do
--   putStrLn "Enter your name:"
--   s <- getLine
--   putStrLn (someFunc s)
