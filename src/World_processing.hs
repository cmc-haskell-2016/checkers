module World_processing (create_checkers_object, is_there_checker) where

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
-- this function creates a list of 1 to 4 positions that are near the checker
nearest_positions :: Checkerboard_pos->[Checkerboard_pos]
nearest_positions x | (mod x 8) < 5 = filter (< 33) (filter (> 0) [x-5,x-4,x+3,x+4])
                  | otherwise = filter (< 33) (filter (> 0)[x-4,x-3,x+4,x+5])


-- searches for a pos in a list and returns true if finds it
check_pos :: Checkerboard_pos-> [Checker]->Bool
check_pos x ch = (foldr (||) False (map (\(pos, alive, _) -> ((x == pos) && alive)) ch))
-- finds all the nearest pos except pos, that has a playable checker there
nearest_reachable_pos :: [Checkerboard_pos]->[Checker]->[Checkerboard_pos]
nearest_reachable_pos [] _ = []
nearest_reachable_pos  (x:xs) list | (check_pos x list) = nearest_reachable_pos xs list
                                | otherwise = x : (nearest_reachable_pos xs list)
--finds all unplayable nearest checker
nearest_unplayable_pos :: [Checkerboard_pos]->[Checker]->[Checkerboard_pos]
nearest_unplayable_pos [] _ = []
nearest_unplayable_pos (x:xs) list | (check_pos x list) = x:nearest_unplayable_pos xs list
                                     | otherwise = nearest_unplayable_pos xs list

-- transform list into list of ways (example: [1,2,3] ==> [([1],[]),([2],[]),([3],[])]    )
make_poslist :: [Checkerboard_pos]->[Way]
make_poslist [] = []
make_poslist (x:xs) = ([x],[]) : (make_poslist xs)

playable_checker :: Checkers -> Int -> [Checker]
playable_checker (first, second) stateId = if stateId == 1 then first else second

unplayable_checker :: Checkers -> Int -> [Checker]
unplayable_checker (first, second) stateId = if stateId == 1 then second else first

-- List of possible moves from the position given as the first argument
possible_moves :: Checkerboard_pos -> Checkers -> State -> [Way]
possible_moves x ch_cortege (stateId, _, _, _) | playable == [] = []
                                               | unplayable == [] = make_poslist playable
                               | otherwise = undefined
                                   where playable = (nearest_reachable_pos (nearest_positions x) (playable_checker ch_cortege stateId)) -- !!!!the list of all positions around except those where stayed another playable checkers
                                         unplayable = (nearest_unplayable_pos playable (playable_checker ch_cortege stateId))-- !!!!the list of all the unplayable checkers that can be eaten


-- Makes a move (may be recursive) and builds new Checkers object
make_move :: Checkerboard_pos -> Checkerboard_pos -> Checkers -> State -> Checkers
make_move = undefined
