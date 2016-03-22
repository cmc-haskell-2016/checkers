
-- | Display "Hello World" in a window.
--
import Graphics.Gloss
import Codec.BMP
import System.IO.Unsafe

main :: IO ()
main
 = display
         (InWindow
         "Hello World"  -- window title
    (800, 600)   -- window size
    (10, 10))  -- window position
  white -- background color
  (Pictures all_pictures) -- picture to display

all_pictures :: [Picture]
all_pictures
  = [picture_bmp, picture1, picture2]

picture1 :: Picture
picture1
  = Translate (-170) (-20) -- shift the text to the middle of the window
  $ Scale 0.5 0.5  -- display it half the original size
  $ Color red
  $ Text "Hello World"  -- text to display

picture2 :: Picture
picture2
   = Translate (0) (-10)
   $ Scale 1 1
   $ ThickCircle 15 15

picture_bmp :: Picture
picture_bmp
  = Translate (0) (-10) -- shift the text to the middle of the window
  $ Scale 2 2  -- display it double the original size
  $ unsafePerformIO (loadBMP "data/bg.bmp")

cell :: Int -> Float -> Float -> Picture -- cell_type -> x -> y -> result picture
cell
 _ x y = Translate (x) (y)
        $ Scale 1 1
        $ unsafePerformIO (loadBMP "data/bg.bmp")


add_row :: [Picture] -> Int -> Float -> Float -> [Picture] -- input picture list -> iterations left -> x -> y -> result picture list
add_row pic_list 0 _ _ = pic_list
add_row pic_list n x y = (cell 0 x y):(add_row (pic_list) (n - 1) (x + 100) y)

-- add_board :: [Picture] -> [Picture]
-- add_promt :: [Picture] -> [Picture]
