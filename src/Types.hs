module Types where

type CheckerboardPos = Int

-- first is for all the positions, that checker go, second is for all eaten checkers positions
type Way = ([CheckerboardPos], [CheckerboardPos])
type Way2 = [(CheckerboardPos, CheckerboardPos)]

data WorldObject = WorldObject { players :: Players
                               , checkers :: Checkers
                               , state :: State
                               , alertMessage :: String
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

data Players = Players { fstPlayerData :: PlayerData
                        , sndPlayerData :: PlayerData
                        }

data PlayerData = PlayerData { isGuest :: Bool
                             , login :: String
                             , password :: String
                             , stats :: PlayerStats
                             }
data PlayerStats = PlayerStats { moves :: Int
                               , wins :: Int
                               , draws :: Int
                               , losts :: Int
                               }

data TopTable = TopTable {records :: [TopTableRecord]}

data TopTableRecord = TopTableRecord { name :: String
                                     , statsFromTable :: PlayerStats
                                     }
-- stateId == 0: snd player`s move
-- stateId == 1: fst player`s move
-- stateId == 2: promo screen
