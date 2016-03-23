module Graphics_processing (game_start) where

import Types

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Codec.BMP
import System.IO.Unsafe

game_start :: IO ()
game_start = play display_mode bg_color steps_per_second initial_world world_to_picture event_handler sim_step

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

--------------------

world_elements :: [Picture]
world_elements
  = add_board 8 False (-300) (200)

add_board :: Int -> Bool -> Float -> Float -> [Picture] -- rows left -> black_needed -> x y -> result picture list
add_board 0 _ _ _ = []
add_board n black_needed x y = (add_row [] 8 black_needed x y) ++ (add_board (n - 1) (not black_needed) x (y - cell_offset))

-- input picture list -> iterations left -> black cell needed -> x -> y -> result picture list
add_row :: [Picture] -> Int -> Bool -> Float -> Float -> [Picture]
add_row pic_list 0 _ _ _ = pic_list
add_row pic_list n black_needed x y = (add_cell black_needed x y):(add_row (pic_list) (n - 1) (not black_needed) (x + cell_offset) y)

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

cell_offset :: Float
cell_offset = 50

add_cell :: Bool -> Float -> Float -> Picture -- cell is black -> x -> y -> result picture
add_cell True x y = Translate (x) (y)
  $ Scale 0.5 0.5
  $ unsafePerformIO (loadBMP "data/black.bmp")
add_cell False x y = Translate (x) (y)
  $ Scale 0.5 0.5
  $ unsafePerformIO (loadBMP "data/white.bmp")
