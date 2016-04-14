module Types where

type WorldObject = (Checkers, State, AlertMessage)
type State = (Int, Int, Int, Bool) -- player id, checker chosen, position to move chosen, if checker has already been chosen
type AlertMessage = String
type Checkers = ([Checker], [Checker]) -- white and black checkers (won't change their order)
type Checker = (CheckerboardPos, IsAlive, IsKing)
type CheckerboardPos = Int
type IsAlive = Bool
type IsKing = Bool

type Way = ([CheckerboardPos], [CheckerboardPos]) --first is for all the positions, that checker go, second is for all eaten checkers positions
type Way2 = [(CheckerboardPos, CheckerboardPos)]
