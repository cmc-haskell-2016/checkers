module World_processing (create_checkers_object, is_there_checker, game_move, if_king, ifLightened) where

import Types

-- Initial version of the board configuration
-- this function creates a list of Checker (for example f 1 3 ==> [(1,True,False),(2,True,False),(3,True,False)]
create_consistent_list :: Checkerboard_pos->Checkerboard_pos->[Checker]
create_consistent_list first last | first > last = []
                                  | otherwise = (first,True, False):create_consistent_list (first + 1) last

-- creating the Checkers - fst Checkers ==> white, snd Checkers ==> black
create_checkers_object :: Checkers
create_checkers_object =((create_consistent_list 1 12), (create_consistent_list 21 32))

-- Testing if there is a checker of the player in the board position
is_there_checker :: Checkerboard_pos -> Checkers -> State -> Bool
is_there_checker pos checker_set (stateId, _, _, _) =
                                 (any (\(checkerPos, isAlive, _) -> pos == checkerPos && isAlive) (id_func checker_set))
                                 --states whether there is a living checker with a given position of an appropriate colour
                                           where id_func = if stateId == 1 then fst else snd
                                           --makes tuple extraction function depending on stateId
--   /
--  <
left_down :: Checkerboard_pos -> Checkerboard_pos
left_down x | (x == 5) || (x == 13) || (x == 21) = 0
            | (x == 29) || (x == 30) || (x == 31) || (x == 32) = 0
            | (mod x 8) < 5 && (mod x 8) > 0 = x+4
            | otherwise = x+3
-- <
--  \
left_up :: Checkerboard_pos -> Checkerboard_pos
left_up x | (x == 5) || (x == 13) || (x == 21) || (x == 29) = 0
          | (x == 1) || (x == 2) || (x == 3) || (x == 4) = 0
            | (mod x 8) < 5 && (mod x 8) > 0 = x-4
            | otherwise = x-5
--  >
-- /
right_up :: Checkerboard_pos -> Checkerboard_pos
right_up x | (x == 12) || (x == 20) || (x == 28) = 0
          | (x == 1) || (x == 2) || (x == 3) || (x == 4) = 0
          | (mod x 8) < 5 && (mod x 8) > 0 = x-3
          | otherwise = x-4
-- \
--  >
right_down :: Checkerboard_pos -> Checkerboard_pos
right_down x | (x == 4) || (x == 12) || (x == 20) || (x == 28) = 0
          | (x == 29) || (x == 30) || (x == 31) || (x == 32) = 0
          | (mod x 8) < 5 && (mod x 8) > 0 = x+5
          | otherwise = x+4

eat_checker :: (Checkerboard_pos -> Checkerboard_pos) -> Checkerboard_pos -> (Checkerboard_pos, Checkerboard_pos)
eat_checker f x | pos /= 0 = (pos, f pos)
                | otherwise = (0,0)
                where pos = (f x)

-- this function creates a list of 1 to 4 positions that are near the checker
nearest_positions :: Checkerboard_pos->[Checkerboard_pos]
nearest_positions x = (filter (> 0) [(left_down x),(left_up x),(right_up x),(right_down x)])

-- creates a list of 0 to 2 pos, where can go (not eat) the checker
position_to_go :: Int -> Checkerboard_pos -> [Checkerboard_pos]
position_to_go id pos | id == 1 = (filter (> 0) [(left_down pos), (right_down pos)])
                      | otherwise = (filter (> 0) [(left_up pos), (right_up pos)])

-- searches for a pos in a list and returns true if finds it
check_pos :: Checkerboard_pos-> [Checker]->Bool
check_pos x ch = (foldr (||) False (map (\(pos, alive, _) -> ((x == pos) && alive)) ch))


-- finds all the nearest pos except pos, that has a playable checker there
nearest_reachable_pos :: [Checkerboard_pos]->[Checker]->[Checker]->[Checkerboard_pos]
nearest_reachable_pos [] _ _ = []
nearest_reachable_pos  (x:xs) list_play list_unplay | (check_pos x list_play) || (check_pos x list_unplay) = nearest_reachable_pos xs list_play list_unplay
                                                    | otherwise = x : (nearest_reachable_pos xs list_play list_unplay)
--finds all unplayable nearest checker
nearest_unplayable_pos :: [Checkerboard_pos]->[Checker]->[Checkerboard_pos]
nearest_unplayable_pos [] _ = []
nearest_unplayable_pos (x:xs) list | (check_pos x list) = x:nearest_unplayable_pos xs list
                                     | otherwise = nearest_unplayable_pos xs list

can_be_eaten_pos :: (Checkerboard_pos, Checkerboard_pos) -> [Checker] -> [Checker] -> (Checkerboard_pos, Checkerboard_pos)
can_be_eaten_pos  (x,y) list_play list_unplay | x == 0 = (x,y)
                                              | y == 0 = (0,y)
                                              | (check_pos x list_unplay) &&  not (check_pos y list_unplay) && not (check_pos y list_play) = (x,y)
                                              | otherwise = (0,0)

list_of_eaten :: Checkerboard_pos -> [Checker] -> [Checker] -> Way2
list_of_eaten    x  list_play list_unplay = [(can_be_eaten_pos (eat_checker left_down x) list_play list_unplay),(can_be_eaten_pos (eat_checker left_up x) list_play list_unplay), (can_be_eaten_pos (eat_checker right_down x) list_play list_unplay), (can_be_eaten_pos (eat_checker right_up x) list_play list_unplay)]

-- transform list into list of ways (example: [1,2,3] ==> [([1],[]),([2],[]),([3],[])]    )
make_poslist :: [Checkerboard_pos]->[Way]
make_poslist [] = []
make_poslist (x:xs) = ([x],[]) : (make_poslist xs)

make_poslist_eat :: Way2 -> [Way]
make_poslist_eat [] = []
make_poslist_eat ((x,y):xs) = ([y],[x]):(make_poslist_eat xs)

king_make_poslist :: Way2 -> [Way]
king_make_poslist [] = []
king_make_poslist ((x,y):xs) | y == 0 = ([x], []):(king_make_poslist xs)
                             | otherwise = ([y], [x]):(king_make_poslist xs)

playable_checker :: Checkers -> Int -> [Checker]
playable_checker (first, second) stateId = if stateId == 1 then first else second

unplayable_checker :: Checkers -> Int -> [Checker]
unplayable_checker (first, second) stateId = if stateId == 1 then second else first

if_zero :: Way2 -> Bool
if_zero list = (foldr (||) False (map (\(x, _) -> (x /= 0)) list))

if_king2 :: Checkerboard_pos-> [Checker]->Bool
if_king2 x ch = (foldr (||) False (map (\(pos, _, king) -> ((x == pos) && king)) ch))

if_king :: Checkerboard_pos ->  Checkers -> State -> Bool
if_king x ch_cortege (stateId, _, _, _) = if_king2 x (playable_checker ch_cortege stateId)

-- List of possible moves from the position given as the first argument
checker_possible_moves :: Checkerboard_pos -> Checkers -> State -> [Way]
checker_possible_moves x ch_cortege (stateId, _, _, _) | not (if_zero unplayable) = make_poslist playable
                                               | otherwise = make_poslist_eat unplayable
                                               where playable = (nearest_reachable_pos (position_to_go stateId x) (playable_checker ch_cortege stateId) (unplayable_checker ch_cortege stateId)) -- !!!!the list of all positions around except those where stayed another playable checkers
                                                     unplayable = (list_of_eaten x  (playable_checker ch_cortege stateId) (unplayable_checker ch_cortege stateId)) -- !!!!the list of all the unplayable checkers that can be eaten
                                                     --unplayable = (list_of_eaten x  (playable_checker ch_cortege stateId) (unplayable_checker ch_cortege stateId))

king_maybe_can_go ::(Checkerboard_pos -> Checkerboard_pos) -> Checkerboard_pos -> [Checkerboard_pos]
king_maybe_can_go f x | (pos == 0) = []
                      | otherwise = pos : (king_maybe_can_go f pos)
                      where pos = (f x)

king_make_list_of_moves :: Checkerboard_pos -> [Checker] -> [Checker] -> Way2
king_make_list_of_moves x l_player l_unplayer= (king_can_go (king_maybe_can_go left_down x) l_player l_unplayer) ++ (king_can_go (king_maybe_can_go left_up x) l_player l_unplayer)++ (king_can_go (king_maybe_can_go right_up x) l_player l_unplayer) ++ (king_can_go (king_maybe_can_go right_down x) l_player l_unplayer)--

king_can_go :: [Checkerboard_pos] -> [Checker] -> [Checker] -> Way2
king_can_go [] _ _ = []
king_can_go [x] list_play list_unplay = if ((check_pos x list_play) || (check_pos x list_unplay)) then [] else [(x,0)]
king_can_go (x:(y:ys)) list_play list_unplay | (check_pos x list_play) = []
                                             | (check_pos x list_unplay) && not (check_pos y list_unplay) && not (check_pos y list_play) = [(x,y)]
                                             | (check_pos x list_unplay) = []
                                             | otherwise = (x, 0) : (king_can_go (y:ys) list_play list_unplay)

possible_moves_king :: Checkerboard_pos ->  Checkers -> State -> [Way]
possible_moves_king x ch_cortege (stateId, _, _, _) = (king_make_poslist (king_make_list_of_moves x (playable_checker ch_cortege stateId) (unplayable_checker ch_cortege stateId)))

possible_moves :: Checkerboard_pos ->  Checkers -> State -> [Way]
possible_moves x ch_cortege  stateId | (if_king  x ch_cortege  stateId) = possible_moves_king   x ch_cortege  stateId
                                         | otherwise =  checker_possible_moves x ch_cortege  stateId

isOnIdEdge :: Checkerboard_pos -> Int -> Bool
isOnIdEdge pos playerId
        | playerId == 1 && pos >= 29 = True
        | playerId /= 1 && pos <= 4 = True
        | otherwise = False

--actually shiftes a checker according to the player's move
change_pos :: Checkerboard_pos -> Checkerboard_pos -> Int -> Checkers -> Checkers
change_pos start_pos dist_pos playerId checker_set
                | playerId == 1 =
                      (map
                        (\(pos, isAlive, isKing) -> if pos == start_pos
                                                        then (dist_pos, True, isKing || isOnIdEdge dist_pos playerId)
                                                        else (pos, isAlive, isKing)
                        )
                        (fst checker_set),
                       snd checker_set)
                | otherwise =
                      (fst checker_set,
                        map
                        (\(pos, isAlive, isKing) -> if pos == start_pos
                                                        then (dist_pos, True, isKing || isOnIdEdge dist_pos playerId)
                                                        else (pos, isAlive, isKing)
                        )
                        (snd checker_set))

--takes an eaten checker away from the board (changes it's Is_alive attribute)
kill_eaten :: Checkerboard_pos -> Int -> Checkers -> Checkers
kill_eaten posToKill playerId checker_set
                | playerId /= 1 =
                      (map
                        (\(pos, isAlive, isKing) -> if pos == posToKill
                                                                        then (pos, False, isKing)
                                                                        else (pos, isAlive, isKing)
                        )
                        (fst checker_set),
                       snd checker_set)
                | otherwise =
                      (fst checker_set,
                        map
                        (\(pos, isAlive, isKing) -> if pos == posToKill
                                                                        then (pos, False, isKing)
                                                                        else (pos, isAlive, isKing)
                        )
                        (snd checker_set)
                       )

--makes a move from a consistent start_pos to a yet-to-be-checked dest_pos
move_from_to :: Checkerboard_pos -> Checkerboard_pos -> Checkers -> [Way] -> State -> World_object
move_from_to start_pos dest_pos checker_set ways (playerId, checkerChosen, posToMove, ifChosen)
        | list == [] = (checker_set, (playerId, checkerChosen, 0, False), "Can not move checker to this position")
        | otherwise = if (snd (head list) == [])
                            then (moved_board, ((playerId + 1) `mod` 2, 1, 1, False), "")
                            else (kill_eaten (head (snd (head list))) playerId moved_board,
                                  ((playerId + 1) `mod` 2, 1, 1, False),
                                  ""
                                 )
            where list = filter (\(xs, _) -> (head xs) == dest_pos) ways
                 -- id_func1 = if playerId == 1 then fst else snd
                  --id_func2 = if playerId == 1 then snd else fst
                  moved_board = change_pos start_pos dest_pos playerId checker_set;

-- Makes a move and builds new World object
make_move :: Checkerboard_pos -> Checkerboard_pos -> Checkers -> State -> World_object
make_move start_pos dest_pos checker_set (playerId, checkerChosen, posToMove, ifChosen)
        | (is_there_checker start_pos checker_set (playerId, checkerChosen, posToMove, ifChosen)) =
                    move_from_to
                        start_pos
                        dest_pos
                        checker_set
                        (possible_moves start_pos checker_set (playerId, checkerChosen, posToMove, ifChosen))
                        (playerId, checkerChosen, posToMove, ifChosen)
        | otherwise = (checker_set, (playerId, 1, 1, False), "Can not take checker from this position")

if_player_has_moves :: Int -> Checkers -> Bool
if_player_has_moves playerId checker_set
        | any (\x -> x /= []) list = True
        | otherwise = False
        where list = map
                        (\(pos, isAlive, _) -> if isAlive
                                                then possible_moves pos checker_set (playerId, undefined, undefined, undefined)
                                                else [] )
                        (if playerId == 1 then fst checker_set else snd checker_set)

if_game_over :: World_object -> Bool
if_game_over (checker_set, (playerId, _, _, _), _)
        | if_player_has_moves playerId checker_set &&
            (any (\(_, isAlive, _) -> isAlive) (if playerId == 1 then (snd checker_set) else (fst checker_set)) )
             = False
        | otherwise = True
           -- where id_func = if playerId == 1 then fst else snd

game_move :: World_object -> World_object
game_move (checker_set, (playerId, checkerChosen, posToMove, ifChosen), _) =
            if if_game_over (checker_set, (playerId, checkerChosen, posToMove, ifChosen), "")
                    then (checker_set, (playerId, checkerChosen, posToMove, ifChosen), "Player " ++ show playerId ++ " lost")
                    else make_move checkerChosen posToMove checker_set (playerId, checkerChosen, posToMove, ifChosen)

--takes current position, position to check, board and state
ifLightened :: Checkerboard_pos -> Checkerboard_pos -> Checkers -> State -> Bool
ifLightened start_pos dist_pos checker_set state = any (\(xs, _) -> (head xs) == dist_pos) ways
            where ways = possible_moves start_pos checker_set state
