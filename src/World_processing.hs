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

-- List of possible moves from the position given as the first argument
possible_moves :: Checkerboard_pos -> Checkers -> State -> [Checkerboard_pos]
possible_moves = undefined

-- Makes a move (may be recursive) and builds new Checkers object
make_move :: Checkerboard_pos -> Checkerboard_pos -> Checkers -> State -> Checkers
make_move = undefined
