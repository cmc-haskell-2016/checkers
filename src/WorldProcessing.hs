module WorldProcessing (createCheckersObject, isThereChecker, gameMove, ifKing, ifLightened) where

import Types

-- Initial version of the board configuration
-- this function creates a list of Checker (for example f 1 3 ==> [(1,True,False),(2,True,False),(3,True,False)]
createConsistentList :: CheckerboardPos->CheckerboardPos->[Checker]
createConsistentList first last | first > last = []
                                  | otherwise = (Checker first True False):createConsistentList (first + 1) last

-- creating the Checkers - fst Checkers ==> white, snd Checkers ==> black
createCheckersObject :: Checkers
createCheckersObject =(Checkers (createConsistentList 1 12) (createConsistentList 21 32))

-- Testing if there is a checker of the player in the board position
isThereChecker :: CheckerboardPos -> Checkers -> State -> Bool
isThereChecker pos checkerSet (State stateId _ _ _) =
                                 (any (\(Checker checkerPos isAlive _) -> pos == checkerPos && isAlive) (idFunc checkerSet))
                                 --states whether there is a living checker with a given position of an appropriate colour
                                           where idFunc = if stateId == 1 then fstPlayer else sndPlayer
                                           --makes tuple extraction function depending on stateId
--   /
--  <
leftDown :: CheckerboardPos -> CheckerboardPos
leftDown x | (x == 5) || (x == 13) || (x == 21) = 0
            | (x == 29) || (x == 30) || (x == 31) || (x == 32) = 0
            | (mod x 8) < 5 && (mod x 8) > 0 = x+4
            | otherwise = x+3
-- <
--  \
leftUp :: CheckerboardPos -> CheckerboardPos
leftUp x | (x == 5) || (x == 13) || (x == 21) || (x == 29) = 0
          | (x == 1) || (x == 2) || (x == 3) || (x == 4) = 0
            | (mod x 8) < 5 && (mod x 8) > 0 = x-4
            | otherwise = x-5
--  >
-- /
rightUp :: CheckerboardPos -> CheckerboardPos
rightUp x | (x == 12) || (x == 20) || (x == 28) = 0
          | (x == 1) || (x == 2) || (x == 3) || (x == 4) = 0
          | (mod x 8) < 5 && (mod x 8) > 0 = x-3
          | otherwise = x-4
-- \
--  >
rightDown :: CheckerboardPos -> CheckerboardPos
rightDown x | (x == 4) || (x == 12) || (x == 20) || (x == 28) = 0
          | (x == 29) || (x == 30) || (x == 31) || (x == 32) = 0
          | (mod x 8) < 5 && (mod x 8) > 0 = x+5
          | otherwise = x+4

eatChecker :: (CheckerboardPos -> CheckerboardPos) -> CheckerboardPos -> (CheckerboardPos, CheckerboardPos)
eatChecker f x | pos /= 0 = (pos, f pos)
                | otherwise = (0,0)
                where pos = (f x)

-- this function creates a list of 1 to 4 positions that are near the checker
nearestPositions :: CheckerboardPos->[CheckerboardPos]
nearestPositions x = (filter (> 0) [(leftDown x),(leftUp x),(rightUp x),(rightDown x)])

-- creates a list of 0 to 2 pos, where can go (not eat) the checker
positionToGo :: Int -> CheckerboardPos -> [CheckerboardPos]
positionToGo id pos | id == 1 = (filter (> 0) [(leftDown pos), (rightDown pos)])
                      | otherwise = (filter (> 0) [(leftUp pos), (rightUp pos)])

-- searches for a pos in a list and returns true if finds it
checkPos :: CheckerboardPos-> [Checker]->Bool
checkPos x ch = (foldr (||) False (map (\(Checker pos alive _) -> ((x == pos) && alive)) ch))


-- finds all the nearest pos except pos, that has a playable checker there
nearestReachablePos :: [CheckerboardPos]->[Checker]->[Checker]->[CheckerboardPos]
nearestReachablePos [] _ _ = []
nearestReachablePos  (x:xs) listPlay listUnplay | (checkPos x listPlay) || (checkPos x listUnplay) = nearestReachablePos xs listPlay listUnplay
                                                    | otherwise = x : (nearestReachablePos xs listPlay listUnplay)
--finds all unplayable nearest checker
nearestUnplayablePos :: [CheckerboardPos]->[Checker]->[CheckerboardPos]
nearestUnplayablePos [] _ = []
nearestUnplayablePos (x:xs) list | (checkPos x list) = x:nearestUnplayablePos xs list
                                     | otherwise = nearestUnplayablePos xs list

canBeEatenPos :: (CheckerboardPos, CheckerboardPos) -> [Checker] -> [Checker] -> (CheckerboardPos, CheckerboardPos)
canBeEatenPos  (x,y) listPlay listUnplay | x == 0 = (x,y)
                                              | y == 0 = (0,y)
                                              | (checkPos x listUnplay) &&  not (checkPos y listUnplay) && not (checkPos y listPlay) = (x,y)
                                              | otherwise = (0,0)

listOfEaten :: CheckerboardPos -> [Checker] -> [Checker] -> Way2
listOfEaten    x  listPlay listUnplay = [(canBeEatenPos (eatChecker leftDown x) listPlay listUnplay),(canBeEatenPos (eatChecker leftUp x) listPlay listUnplay), (canBeEatenPos (eatChecker rightDown x) listPlay listUnplay), (canBeEatenPos (eatChecker rightUp x) listPlay listUnplay)]

-- transform list into list of ways (example: [1,2,3] ==> [([1],[]),([2],[]),([3],[])]    )
makePoslist :: [CheckerboardPos]->[Way]
makePoslist [] = []
makePoslist (x:xs) = ([x],[]) : (makePoslist xs)

makePoslistEat :: Way2 -> [Way]
makePoslistEat [] = []
makePoslistEat ((x,y):xs) = ([y],[x]):(makePoslistEat xs)

kingMakePoslist :: Way2 -> [Way]
kingMakePoslist [] = []
kingMakePoslist ((x,y):xs) | y == 0 = ([x], []):(kingMakePoslist xs)
                             | otherwise = ([y], [x]):(kingMakePoslist xs)

playableChecker :: Checkers -> Int -> [Checker]
playableChecker (Checkers first second) stateId = if stateId == 1 then first else second

unplayableChecker :: Checkers -> Int -> [Checker]
unplayableChecker (Checkers first second) stateId = if stateId == 1 then second else first

ifZero :: Way2 -> Bool
ifZero list = (foldr (||) False (map (\(x, _) -> (x /= 0)) list))

ifKing2 :: CheckerboardPos-> [Checker]->Bool
ifKing2 x ch = (foldr (||) False (map (\(Checker pos _ king) -> ((x == pos) && king)) ch))

ifKing :: CheckerboardPos ->  Checkers -> State -> Bool
ifKing x chCortege (State stateId _ _ _) = ifKing2 x (playableChecker chCortege stateId)

-- List of possible moves from the position given as the first argument
checkerPossibleMoves :: CheckerboardPos -> Checkers -> State -> [Way]
checkerPossibleMoves x chCortege (State stateId _ _ _) | not (ifZero unplayable) = makePoslist playable
                                               | otherwise = makePoslistEat unplayable
                                               where playable = (nearestReachablePos (positionToGo stateId x) (playableChecker chCortege stateId) (unplayableChecker chCortege stateId)) -- !!!!the list of all positions around except those where stayed another playable checkers
                                                     unplayable = (listOfEaten x  (playableChecker chCortege stateId) (unplayableChecker chCortege stateId)) -- !!!!the list of all the unplayable checkers that can be eaten
                                                     --unplayable = (listOfEaten x  (playableChecker chCortege stateId) (unplayableChecker chCortege stateId))

kingMaybeCanGo ::(CheckerboardPos -> CheckerboardPos) -> CheckerboardPos -> [CheckerboardPos]
kingMaybeCanGo f x | (pos == 0) = []
                      | otherwise = pos : (kingMaybeCanGo f pos)
                      where pos = (f x)

kingMakeListOfMoves :: CheckerboardPos -> [Checker] -> [Checker] -> Way2
kingMakeListOfMoves x lPlayer lUnplayer= (kingCanGo (kingMaybeCanGo leftDown x) lPlayer lUnplayer) ++ (kingCanGo (kingMaybeCanGo leftUp x) lPlayer lUnplayer) ++ (kingCanGo (kingMaybeCanGo rightUp x) lPlayer lUnplayer) ++ (kingCanGo (kingMaybeCanGo rightDown x) lPlayer lUnplayer)--

kingCanGo :: [CheckerboardPos] -> [Checker] -> [Checker] -> Way2
kingCanGo [] _ _ = []
kingCanGo [x] listPlay listUnplay = if ((checkPos x listPlay) || (checkPos x listUnplay)) then [] else [(x,0)]
kingCanGo (x:(y:ys)) listPlay listUnplay | (checkPos x listPlay) = []
                                             | (checkPos x listUnplay) && not (checkPos y listUnplay) && not (checkPos y listPlay) = [(x,y)]
                                             | (checkPos x listUnplay) = []
                                             | otherwise = (x, 0) : (kingCanGo (y:ys) listPlay listUnplay)

possibleMovesKing :: CheckerboardPos ->  Checkers -> State -> [Way]
possibleMovesKing x chCortege (State stateId _ _ _) = (kingMakePoslist (kingMakeListOfMoves x (playableChecker chCortege stateId) (unplayableChecker chCortege stateId)))

possibleMoves :: CheckerboardPos ->  Checkers -> State -> [Way]
possibleMoves x chCortege  stateId | (ifKing  x chCortege  stateId) = possibleMovesKing   x chCortege  stateId
                                         | otherwise =  checkerPossibleMoves x chCortege  stateId

isOnIdEdge :: CheckerboardPos -> Int -> Bool
isOnIdEdge pos playerId
        | playerId == 1 && pos >= 29 = True
        | playerId /= 1 && pos <= 4 = True
        | otherwise = False

--actually shiftes a checker according to the player's move
changePos :: CheckerboardPos -> CheckerboardPos -> Int -> Checkers -> Checkers
changePos startPos distPos playerId checkerSet
                | playerId == 1 =
                      (Checkers (map
                        (\(Checker pos isAlive isKing) -> if pos == startPos
                                                        then (Checker distPos True (isKing || isOnIdEdge distPos playerId))
                                                        else (Checker pos isAlive isKing)
                        )
                        (fstPlayer checkerSet)) (sndPlayer checkerSet))
                | otherwise =
                      (Checkers (fstPlayer checkerSet)
                        (map
                        (\(Checker pos isAlive isKing) -> if pos == startPos
                                                        then (Checker distPos True (isKing || isOnIdEdge distPos playerId))
                                                        else (Checker pos isAlive isKing)
                        )
                        (sndPlayer checkerSet)))

--takes an eaten checker away from the board (changes it's IsAlive attribute)
killEaten :: CheckerboardPos -> Int -> Checkers -> Checkers
killEaten posToKill playerId checkerSet
                | playerId /= 1 =
                      (Checkers (map
                        (\(Checker pos isAlive isKing) -> if pos == posToKill
                                                                        then (Checker pos False isKing)
                                                                        else (Checker pos isAlive isKing)
                        )
                        (fstPlayer checkerSet))
                       (sndPlayer checkerSet))
                | otherwise =
                      (Checkers (fstPlayer checkerSet)
                        (map
                        (\(Checker pos isAlive isKing) -> if pos == posToKill
                                                                        then (Checker pos False isKing)
                                                                        else (Checker pos isAlive isKing)
                        )
                        (sndPlayer checkerSet))
                       )

--makes a move from a consistent startPos to a yet-to-be-checked destPos
moveFromTo :: CheckerboardPos -> CheckerboardPos -> Checkers -> [Way] -> State -> WorldObject
moveFromTo startPos destPos checkerSet ways (State playerId checkerChosen posToMove ifChosen)
        | list == [] = (WorldObject checkerSet (State playerId checkerChosen 0 False) "Can not move checker to this position")
        | otherwise = if (snd (head list) == [])
                            then (WorldObject movedBoard (State ((playerId + 1) `mod` 2) 1 1 False) "")
                            else (WorldObject (killEaten (head (snd (head list))) playerId movedBoard)
                                  (State ((playerId + 1) `mod` 2) 1 1 False) ""
                                 )
            where list = filter (\(xs, _) -> (head xs) == destPos) ways
                 -- idFunc1 = if playerId == 1 then fst else snd
                  --idFunc2 = if playerId == 1 then snd else fst
                  movedBoard = changePos startPos destPos playerId checkerSet;

-- Makes a move and builds new World object
makeMove :: CheckerboardPos -> CheckerboardPos -> Checkers -> State -> WorldObject
makeMove startPos destPos checkerSet (State playerId checkerChosen posToMove ifChosen)
        | (isThereChecker startPos checkerSet (State playerId checkerChosen posToMove ifChosen)) =
                    moveFromTo
                        startPos
                        destPos
                        checkerSet
                        (possibleMoves startPos checkerSet (State playerId checkerChosen posToMove ifChosen))
                        (State playerId checkerChosen posToMove ifChosen)
        | otherwise = (WorldObject checkerSet (State playerId 1 1 False) "Can not take checker from this position")

ifPlayerHasMoves :: Int -> Checkers -> Bool
ifPlayerHasMoves playerId checkerSet
        | any (\x -> x /= []) list = True
        | otherwise = False
        where list = map
                        (\(Checker pos isAlive _) -> if isAlive
                                                then possibleMoves pos checkerSet (State playerId undefined undefined undefined)
                                                else [] )
                        (if playerId == 1 then (fstPlayer checkerSet) else (sndPlayer checkerSet))

ifGameOver :: WorldObject -> Bool
ifGameOver (WorldObject checkerSet (State playerId _ _ _) _)
        | ifPlayerHasMoves playerId checkerSet &&
            (any (\(Checker _ isAlive _) -> isAlive) (if playerId == 1 then (sndPlayer checkerSet) else (fstPlayer checkerSet)) )
             = False
        | otherwise = True
           -- where idFunc = if playerId == 1 then fst else snd

gameMove :: WorldObject -> WorldObject
gameMove (WorldObject checkerSet (State playerId checkerChosen posToMove ifChosen) _) =
            if ifGameOver (WorldObject checkerSet (State playerId checkerChosen posToMove ifChosen) "")
                    then (WorldObject checkerSet (State playerId checkerChosen posToMove ifChosen) ("Player " ++ show playerId ++ " lost"))
                    else makeMove checkerChosen posToMove checkerSet (State playerId checkerChosen posToMove ifChosen)

--takes current position, position to check, board and state
ifLightened :: CheckerboardPos -> CheckerboardPos -> Checkers -> State -> Bool
ifLightened startPos distPos checkerSet state = any (\(xs, _) -> (head xs) == distPos) ways
            where ways = possibleMoves startPos checkerSet state
