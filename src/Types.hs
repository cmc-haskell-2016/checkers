module Types where

type World_object = (Checkers, State, Alert_message)
type State = (Int, Int, Int, Bool) -- player id, checker chosen, position to move chosen, if checker has already been chosen
type Alert_message = String
type Checkers = ([Checker], [Checker]) -- white and black checkers (won't change their order)
type Checker = (Checkerboard_pos, Is_alive, Is_king)
type Checkerboard_pos = Int
type Is_alive = Bool
type Is_king = Bool

type Way = ([Checkerboard_pos], [Checkerboard_pos]) --first is for all the positions, that checker go, second is for all eaten checkers positions