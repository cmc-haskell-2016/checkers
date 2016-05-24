module Main where

-- import Lib
import Types
import WorldProcessing
import GraphicsProcessing
import Database (runDb)

main :: IO ()
main = do
  -- runDb
  gameStart
