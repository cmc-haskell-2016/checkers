module Graphics_processing (game_start) where

import Types
import World_processing

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Codec.BMP
import System.IO.Unsafe

type Screen_x_pos = Float
type Screen_y_pos = Float

game_start :: IO ()
game_start = play display_mode bg_color steps_per_second initial_world world_to_picture event_handler sim_step

-- display configuration
display_mode :: Display
display_mode = InWindow
  "Checkers"  -- window title
  (800, 600)   -- window size
  (0, 0) -- window offset

bg_color :: Color
bg_color = makeColor 0.7 0.7 0.8 1

steps_per_second :: Int
steps_per_second = 15

initial_world :: World_object -- will be changed
initial_world = (create_checkers_object, (1, 1, 1, False), "Press any key to start")

world_to_picture :: World_object -> Picture
world_to_picture world = Pictures (world_elements world)

event_handler :: Event -> World_object -> World_object
event_handler (EventKey (SpecialKey KeyUp) Down _ _) (checkers, (player_id, checker_chosen, pos_to_move_chosen, False), alert_message) = (checkers, (player_id, (mod (checker_chosen + 32 - 4) 32), pos_to_move_chosen, False), alert_message)
-- event_handler (EventKey (SpecialKey KeyUp) Down _ _) (checkers, (player_id, checker_chosen, pos_to_move_chosen, False), alert_message) = (checkers, (player_id, (mod (checker_chosen + 32 - 4) 32), pos_to_move_chosen, False), alert_message)
-- event_handler (EventKey (SpecialKey KeyUp) Down _ _) (checkers, (player_id, checker_chosen, pos_to_move_chosen, True), alert_message) = (EventKey (SpecialKey KeyUp) Down _ _) (checkers, (player_id, checker_chosen, (mod (pos_to_move_chosen + 32 - 4) 32), True), alert_message)
event_handler _ w = w

sim_step :: Float -> World_object -> World_object
sim_step _ w = w

-- end of display configuration

world_elements :: World_object -> [Picture]
world_elements (checkers, state, alert_message)
  = (add_board (-300) 200 checkers state) ++ (add_infobar alert_message 200 25)

add_infobar :: Alert_message -> Screen_x_pos -> Screen_y_pos -> [Picture]
add_infobar message x y
  = (infobar_bg x y) : (infobar_title 135 190 "Checkers") : (infobar_text 100 50 message):[]

infobar_title :: Screen_x_pos -> Screen_y_pos -> String -> Picture
infobar_title x y message
  = Translate x y
  $ Scale 0.25 0.25
  $ Color black
  $ Text message


infobar_text :: Screen_x_pos -> Screen_y_pos -> String -> Picture
infobar_text x y message
  = Translate x y
  $ Scale 0.13 0.13
  $ Color black
  $ Text message

infobar_bg :: Screen_x_pos -> Screen_y_pos -> Picture
infobar_bg x y
  = Translate x y
  $ Scale 2 4
  $ unsafePerformIO (loadBMP "data/white.bmp")

------- board drawing functions

-- x_start -> y_start -> result_picture_list
add_board :: Screen_x_pos -> Screen_y_pos -> Checkers -> State -> [Picture]
add_board x y checkers state
  = add_raw 8 False x y checkers state

-- rows left -> black_color -> x -> y -> result_picture_list
add_raw :: Int -> Bool -> Screen_x_pos -> Screen_y_pos -> Checkers -> State -> [Picture]
add_raw 0 _ _ _ _ _ = []
add_raw n black_needed x y checkers state = (add_cell 8 black_needed x y) ++ (add_raw (n - 1) (not black_needed) x (y - cell_offset) checkers state) ++ (add_checkers 4 n x y checkers) ++ (add_numbers 4 n x y state)

-- cell_offset is equal to cell size (because one cell goes just after another in the row)
cell_offset :: Float
cell_offset = 50

-- iterations left -> black_cell_is_needed -> x -> y -> result_picture_list
add_cell :: Int -> Bool -> Screen_x_pos -> Screen_y_pos -> [Picture]
add_cell 0 _ _ _ = []
add_cell n black_needed x y = (cell black_needed x y):(add_cell (n - 1) (not black_needed) (x + cell_offset) y)

-- cell_is_black -> x -> y -> result picture
cell :: Bool -> Screen_x_pos -> Screen_y_pos -> Picture
cell True x y = Translate (x) (y)
  $ Scale 0.5 0.5
  $ unsafePerformIO (loadBMP "data/black.bmp")
cell False x y = Translate (x) (y)
  $ Scale 0.5 0.5
  $ unsafePerformIO (loadBMP "data/white.bmp")

-- offset from top left corner of the cell. Needed for drawing numbers on the cells
number_x_offset :: Screen_x_pos
number_x_offset = -20

number_y_offset :: Screen_y_pos
number_y_offset = 10

-- steps_left -> raw_number -> x -> y -> state
add_numbers :: Int -> Int -> Screen_x_pos -> Screen_y_pos -> State -> [Picture]
add_numbers 0 _ _ _ _ = []
add_numbers steps_left raw_number x y (player_id, first_pos, second_pos, first_chosen)
  = (Translate (x + number_x_offset + if (mod raw_number 2) == 0 then  cell_offset else 0) (y + number_y_offset)
  $ Scale 0.125 0.125
  $ Color red $ Text ((show number)
    ++ (if number == first_pos then "*" else "")
    ++ (if (first_chosen && (number == second_pos)) then "#" else "")))
      : (add_numbers (steps_left - 1) raw_number (x + (2 * cell_offset)) y (player_id, first_pos, second_pos, first_chosen))
      where number = (4 * (8 - raw_number) + (4 - steps_left) + 1)

-- offset from top left corner of the cell. Needed for drawing checkers on the cells
checker_x_offset :: Screen_x_pos
checker_x_offset = 0

checker_y_offset :: Screen_y_pos
checker_y_offset = 0

-- steps_left -> raw_number -> x -> y
add_checkers :: Int -> Int -> Screen_x_pos -> Screen_y_pos -> Checkers -> [Picture]
add_checkers 0 _ _ _ _ = []
add_checkers steps_left raw_number x y checkers
   = (Translate (x + checker_x_offset + if (mod raw_number 2) == 0 then  cell_offset else 0) (y + checker_y_offset)
   $ Scale 1 1
   $ Color (if (is_there_checker number checkers (1, 0, 0, False)) then white else (if (is_there_checker number checkers (2, 0, 0, False)) then bg_color else black))
   $ ThickCircle 12 12)
      : add_checkers (steps_left - 1) raw_number (x + (2 * cell_offset)) y checkers
      where number = (4 * (8 - raw_number) + (4 - steps_left) + 1)

------- end of board drawing functions


-- picture1 :: Picture
-- picture1
--   = Translate (-170) (-20) -- shift the text to the middle of the window
--   $ Scale 0.5 0.5  -- display it half the original size
--   $ Color red
--   $ Text "Hello World"  -- text to display
--
-- picture2 :: Picture
-- picture2
--    = Translate (0) (-10)
--    $ Scale 1 1
--    $ ThickCircle 15 15
--
-- picture_bmp :: Picture
-- picture_bmp
--   = Translate (0) (-10) -- shift the text to the middle of the window
--   $ Scale 2 2  -- display it double the original size
--   $ unsafePerformIO (loadBMP "data/bg.bmp")
