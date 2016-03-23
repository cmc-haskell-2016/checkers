module Main where

import Lib

main :: IO ()
main = do
  putStrLn "Enter your name:"
  s <- getLine
  putStrLn (someFunc s)

type World_object = (Checkers, State, Alert_message)
type State = Int -- 1 - move of the 1st player, 2 - move of the 2nd player
type Alert_message = String
type Checkers = ([Checker], [Checker]) -- white and black checkers (won't change their order)
type Checker = (Checkerboard_pos, Is_alive, Is_king)
type Checkerboard_pos = Int
type Is_alive = Bool
type Is_king = Bool


-- Initial version of the board configuration
create_checkers_object :: Checkers
create_checkers_object = undefined

-- Testing if there is a checker of the player in the board position
is_there_checker :: Checkerboard_pos -> Checkers -> State -> Bool
is_there_checker = undefined

-- List of possible moves from the position given as the first argument
possible_moves :: Checkerboard_pos -> Checkers -> State -> [Checkerboard_pos]
possible_moves = undefined

-- Makes a move (may be recursive) and builds new Checkers object
make_move :: Checkerboard_pos -> Checkerboard_pos -> Checkers -> State -> Checkers
make_move = undefined
