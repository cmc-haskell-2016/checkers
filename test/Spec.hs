
-- | Display "Hello World" in a window.
--
import Graphics.Gloss
import Codec.BMP
import System.IO.Unsafe

main :: IO ()
main
 = display
        (InWindow
	       "Hello World" 	 -- window title
		(400, 150) 	 -- window size
		(10, 10)) 	 -- window position
	white			 -- background color
 	(Pictures all_pictures)			 -- picture to display

all_pictures::[Picture]
all_pictures
 = [picture_bmp, picture1, picture2]

picture1::Picture
picture1
	= Translate (-170) (-20) -- shift the text to the middle of the window
	$ Scale 0.5 0.5		 -- display it half the original size
	$ Text "Hello World"	 -- text to display

picture2::Picture
picture2
  	= Translate (0) (-10) -- shift the text to the middle of the window
  	$ Scale 1 1		 -- display it half the original size
  	$ Circle 15	 -- text to display

picture_bmp:: Picture
picture_bmp
  	= unsafePerformIO (loadBMP "data/bg.bmp")
