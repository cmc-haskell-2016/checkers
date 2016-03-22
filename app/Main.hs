module Main where

import Lib

main :: IO ()
main = do
  putStrLn "Enter your name:"
  s <- getLine
  putStrLn (someFunc s)

type World_object = (Checkers, State, Alert_message)
type Alert_message = String
type State = Int
type Checkers = ([Checker], [Checker]) -- white and black checkers
type Checker = (Checkerboard_pos, Is_alive, Is_king)
type Checkerboard_pos = Int
type Is_alive = Bool
type Is_king = Bool


-- world :: (checkers, state, )
