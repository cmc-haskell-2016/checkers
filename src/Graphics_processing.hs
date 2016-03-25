module Graphics_processing (game_start) where

import Types

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Codec.BMP
import System.IO.Unsafe

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

type World = Int -- will be deleted
initial_world :: World -- will be changed
initial_world = 0

world_to_picture :: World -> Picture
world_to_picture _ = Pictures world_elements

event_handler :: Event -> World -> World
event_handler _ w = w

sim_step :: Float -> World -> World
sim_step _ w = w

-- end of display configuration

world_elements :: [Picture]
world_elements
  = add_board (-300) (200)

-- board drawing functions
add_board :: Float -> Float -> [Picture] -- x -> y -> result picture list
add_board x y
  = add_raw 8 False x y

add_raw :: Int -> Bool -> Float -> Float -> [Picture] -- rows left -> black_needed -> x -> y -> result picture list
add_raw 0 _ _ _ = []
add_raw n black_needed x y = (add_cell 8 black_needed x y) ++ (add_raw (n - 1) (not black_needed) x (y - cell_offset)) ++ (add_numbers 4 n x y)

-- input picture list -> iterations left -> black cell needed -> x -> y -> result picture list
add_cell :: Int -> Bool -> Float -> Float -> [Picture]
add_cell 0 _ _ _ = []
add_cell n black_needed x y = (cell black_needed x y):(add_cell (n - 1) (not black_needed) (x + cell_offset) y)

cell_offset :: Float
cell_offset = 50

cell :: Bool -> Float -> Float -> Picture -- cell_is_black -> x -> y -> result picture
cell True x y = Translate (x) (y)
  $ Scale 0.5 0.5
  $ unsafePerformIO (loadBMP "data/black.bmp")
cell False x y = Translate (x) (y)
  $ Scale 0.5 0.5
  $ unsafePerformIO (loadBMP "data/white.bmp")

add_numbers :: Int -> Int -> Float -> Float -> [Picture] -- steps_left -> raw_number -> x -> y
add_numbers 0 _ _ _ = []
add_numbers steps_left raw_number x y
  = (Translate (if (mod raw_number 2) == 0 then (x + cell_offset - 20) else (x - 20)) (y+10)
  $ Scale 0.125 0.125
  $ Color red $ Text (show (4 * (8 - raw_number) + (4 - steps_left) + 1)))
  : (add_numbers (steps_left - 1) raw_number (x + (2 * cell_offset)) y)


-- end of board drawing functions


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
