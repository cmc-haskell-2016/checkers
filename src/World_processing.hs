module World_processing where

import Types

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
