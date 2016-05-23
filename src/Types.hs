module Types where

type AlertMessage = String
type CheckerboardPos = Int

-- first is for all the positions, that checker go, second is for all eaten checkers positions
type Way = ([CheckerboardPos], [CheckerboardPos])
type Way2 = [(CheckerboardPos, CheckerboardPos)]

data WorldObject = WorldObject { checkers :: Checkers
                               , state :: State
                               , alertMessage :: AlertMessage
                               }

data Checkers = Checkers { fstPlayer :: [Checker]
                         , sndPlayer :: [Checker]
                         }

data Checker = Checker { checkerboardPos :: Int
                       , isAlive :: Bool
                       , isKing :: Bool
                       }

data State = State { stateId :: Int
                   , checkerChosen :: Int
                   , posToMoveChosen :: Int
                   , checkerIsChosen :: Bool
                   }
-- stateId == 0: snd player`s move
-- stateId == 1: fst player`s move
-- stateId == 2: promo screen
