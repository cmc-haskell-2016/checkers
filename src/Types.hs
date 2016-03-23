module Types where

type World_object = (Checkers, State, Alert_message)
type State = Int -- 1 - move of the 1st player, 2 - move of the 2nd player
type Alert_message = String
type Checkers = ([Checker], [Checker]) -- white and black checkers (won't change their order)
type Checker = (Checkerboard_pos, Is_alive, Is_king)
type Checkerboard_pos = Int
type Is_alive = Bool
type Is_king = Bool
