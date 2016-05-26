module GraphicsProcessing (gameStart) where

import Types
import WorldProcessing

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Codec.BMP
import System.IO.Unsafe

import Database (processPlayerName)

type ScreenXPos = Float
type ScreenYPos = Float

gameStart :: IO ()
gameStart = play displayMode bgColor stepsPerSecond initialWorld worldToPicture eventHandler simStep

-- display configuration
displayMode :: Display
displayMode = InWindow
  "Checkers"  -- window title
  (800, 600)   -- window size
  (0, 0) -- window offset

-- just a constant
bgColor :: Color
bgColor = makeColor 0.7 0.7 0.8 1

-- just a constant
stepsPerSecond :: Int
stepsPerSecond = 0

initialWorld :: WorldObject
initialWorld = unsafePerformIO (do
  name <- processPlayerName "Guest"
  return (WorldObject createPlayersObject createCheckersObject (State 2 1 1 False) name))

-- drawing world function
worldToPicture :: WorldObject -> Picture
worldToPicture world = Pictures (worldElements world)

-- calculate coordinate
calculateCoord :: Float -> Float -> Int
calculateCoord x y = (k*4) + (div ((floor x)+205) 100)+1
                      where k = div (150 - (floor y)) 50
--calculateCoord x y | x < -180 && x >= -130 = 10
  --                  | otherwise = 1
-- key pressing handling
eventHandler :: Event -> WorldObject -> WorldObject
-- events before game

-- promo screen. Capture any key
eventHandler (EventKey _ _ _ _) (WorldObject players checkers (State 2 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State 1 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

-- events during game
eventHandler (EventKey (SpecialKey KeyUp) Down _ _) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen False) alertMessage) =
  (WorldObject players checkers (State playerId ((mod (checkerChosen + 31 - 4) 32) + 1) posToMoveChosen False) alertMessage)

eventHandler (EventKey (SpecialKey KeyDown) Down _ _) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen False) alertMessage) =
  (WorldObject players checkers (State playerId ((mod (checkerChosen + 31 + 4) 32) + 1) posToMoveChosen False) alertMessage)

eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen False) alertMessage) =
  (WorldObject players checkers (State playerId ((mod (checkerChosen + 31 - 1) 32) + 1) posToMoveChosen False) alertMessage)

eventHandler (EventKey (SpecialKey KeyRight) Down _ _) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen False) alertMessage) =
  (WorldObject players checkers (State playerId ((mod (checkerChosen + 31 + 1) 32) + 1) posToMoveChosen False) alertMessage)

eventHandler (EventKey (SpecialKey KeyUp) Down _ _) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen True) alertMessage) =
  (WorldObject players checkers (State playerId checkerChosen ((mod (posToMoveChosen + 31 - 4) 32) + 1) True) alertMessage)

eventHandler (EventKey (SpecialKey KeyDown) Down _ _) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen True) alertMessage) =
  (WorldObject players checkers (State playerId checkerChosen ((mod (posToMoveChosen + 31 + 4) 32) + 1) True) alertMessage)

eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen True) alertMessage) =
  (WorldObject players checkers (State playerId checkerChosen ((mod (posToMoveChosen + 31 - 1) 32) + 1) True) alertMessage)

eventHandler (EventKey (SpecialKey KeyRight) Down _ _) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen True) alertMessage) =
  (WorldObject players checkers (State playerId checkerChosen ((mod (posToMoveChosen + 31 + 1) 32) + 1) True) alertMessage)

-- Testing the position if there is a checker of the player to move. Game continues according to the result
eventHandler (EventKey (SpecialKey KeyEnter) Down _ _) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen False) alertMessage) =
  (WorldObject players checkers (State playerId checkerChosen posToMoveChosen True) "Choose position to move checker to")

-- positions are chosen. The turn starts
eventHandler (EventKey (SpecialKey KeyEnter) Down _ _) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen True) alertMessage) =
  gameMove (WorldObject players checkers (State playerId checkerChosen posToMoveChosen False) alertMessage)

-- mouse
eventHandler (EventKey (MouseButton LeftButton) Down _ (x,y)) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen False) alertMessage) =
  (WorldObject players checkers (State playerId (calculateCoord x y) posToMoveChosen True) alertMessage)

eventHandler (EventKey (MouseButton LeftButton) Down _ (x,y)) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen True) alertMessage) =
  gameMove (WorldObject players checkers (State playerId checkerChosen (calculateCoord x y) False) alertMessage)

-- other keys pressed
eventHandler _ w = w

-- the world is static. No need to change it according to the playing time
simStep :: Float -> WorldObject -> WorldObject
simStep _ w = w

-- end of display configuration

-- just constants
boardXOffset :: ScreenXPos
boardXOffset = -180

boardYOffset :: ScreenYPos
boardYOffset = 125

infobarXOffset :: ScreenXPos
infobarXOffset = 0

infobarYOffset :: ScreenYPos
infobarYOffset = 250

-- collecting pictures to display
worldElements :: WorldObject -> [Picture]
worldElements (WorldObject players checkers state alertMessage) | (stateId state) == 2 = (addInfobar state "Press any key" infobarXOffset infobarYOffset)
                                                        | otherwise = (addBoard boardXOffset boardYOffset checkers state)
                                                                      ++ (addInfobar state alertMessage infobarXOffset infobarYOffset)

-- collecting infobar elements
addInfobar :: State -> String -> ScreenXPos -> ScreenYPos -> [Picture]
addInfobar state message x y
  = (infobarBg x y) : (infobarTitle (x - 50) (y + 20) "Checkers") : (infobarText (x - 220) (y - 10) ("move of the player "  ++ (show (2 - (stateId state))))) : (infobarText (x - 220) (y - 50) message) : []

infobarTitle :: ScreenXPos -> ScreenYPos -> String -> Picture
infobarTitle x y message
  = Translate x y
  $ Scale 0.25 0.25
  $ Color black
  $ Text message


infobarText :: ScreenXPos -> ScreenYPos -> String -> Picture
infobarText x y message
  = Translate x y
  $ Scale 0.13 0.13
  $ Color black
  $ Text message

infobarBg :: ScreenXPos -> ScreenYPos -> Picture
infobarBg x y
  = Translate x y
  $ Scale 6 1.5
  $ unsafePerformIO (loadBMP "data/white.bmp")

------- board drawing functions

-- Board raws: x_start -> y_start -> result_picture_list
addBoard :: ScreenXPos -> ScreenYPos -> Checkers -> State -> [Picture]
addBoard x y checkers state
  = addRow 8 False x y checkers state

-- Adding raws recursively. Raw contains cells, checkers numbers on cell: raws left -> black_color -> x -> y -> result_picture_list
addRow :: Int -> Bool -> ScreenXPos -> ScreenYPos -> Checkers -> State -> [Picture]
addRow 0 _ _ _ _ _ = []
addRow n blackNeeded x y checkers state = (addCell 8 blackNeeded x y) ++ (addRow (n - 1) (not blackNeeded) x (y - cellOffset) checkers state) ++ (addCheckers 4 n x y checkers) ++ (addColoring 4 n x y checkers state) ++ (addNumbers 4 n x y state)

-- just a constant cellOffset is equal to cell size (because one cell goes just after another in the row)
cellOffset :: Float
cellOffset = 50

-- Adding cells recursively: iterations left -> black_cell_is_needed -> x -> y -> result_picture_list
addCell :: Int -> Bool -> ScreenXPos -> ScreenYPos -> [Picture]
addCell 0 _ _ _ = []
addCell n blackNeeded x y = (cell blackNeeded x y):(addCell (n - 1) (not blackNeeded) (x + cellOffset) y)

-- Adding one cell: if_cell_is_black -> x -> y -> result picture
cell :: Bool -> ScreenXPos -> ScreenYPos -> Picture
cell True x y = Translate (x) (y)
  $ Scale 0.5 0.5
  $ unsafePerformIO (loadBMP "data/black.bmp")
cell False x y = Translate (x) (y)
  $ Scale 0.5 0.5
  $ unsafePerformIO (loadBMP "data/white.bmp")

-- just constants: offset from top left corner of the cell. Needed for drawing numbers on the cells
numberXOffset :: ScreenXPos
numberXOffset = -20

numberYOffset :: ScreenYPos
numberYOffset = 10

-- Adding numbers recursively: stepsLeft -> rawNumber -> x -> y -> state
addNumbers :: Int -> Int -> ScreenXPos -> ScreenYPos -> State -> [Picture]
addNumbers 0 _ _ _ _ = []
addNumbers stepsLeft rawNumber x y (State playerId firstPos secondPos firstChosen)
  = (Translate (x + numberXOffset + if (mod rawNumber 2) == 0 then  cellOffset else 0) (y + numberYOffset)
  $ Scale 0.125 0.125
  $ Color red $ Text ((show number)
    ++ (if number == firstPos then "*" else "")
    ++ (if (firstChosen && (number == secondPos)) then "#" else "")))
      : (addNumbers (stepsLeft - 1) rawNumber (x + (2 * cellOffset)) y (State playerId firstPos secondPos firstChosen))
      where number = (4 * (8 - rawNumber) + (4 - stepsLeft) + 1)

-- just constants: offset from top left corner of the cell. Needed for drawing checkers on the cells
checkerXOffset :: ScreenXPos
checkerXOffset = 0

checkerYOffset :: ScreenYPos
checkerYOffset = 0

-- Color of the checker construct

whiteCheckerColor :: Bool -> Color
whiteCheckerColor True = makeColor 0.8 1.0 0.8 1
whiteCheckerColor False = white

blackCheckerColor :: Bool -> Color
blackCheckerColor True = makeColor 1.0 0.5 0.5 1
blackCheckerColor False = bgColor

checkerColor :: Checkers -> CheckerboardPos -> Color
checkerColor checkers position =
  if (isThereChecker position checkers (State 1 0 0 False))
    then whiteCheckerColor (ifKing position checkers (State 1 0 0 False))
    else
      (if (isThereChecker position checkers (State 0 0 0 False))
        then blackCheckerColor (ifKing position checkers (State 0 0 0 False))
        else black)

-- Drawing checkers recursively: stepsLeft -> rawNumber -> x -> y
addCheckers :: Int -> Int -> ScreenXPos -> ScreenYPos -> Checkers -> [Picture]
addCheckers 0 _ _ _ _ = []
addCheckers stepsLeft rawNumber x y checkers
   = (Translate (x + checkerXOffset + if (mod rawNumber 2) == 0 then  cellOffset else 0) (y + checkerYOffset)
   $ Scale 1 1
   $ Color (checkerColor checkers number)
   $ ThickCircle 12 12)
      : addCheckers (stepsLeft - 1) rawNumber (x + (2 * cellOffset)) y checkers
      where number = (4 * (8 - rawNumber) + (4 - stepsLeft) + 1)


addColoring :: Int -> Int -> ScreenXPos -> ScreenYPos -> Checkers -> State -> [Picture]
addColoring 0 _ _ _ _ _ = []
addColoring stepsLeft rawNumber x y checkers state
   = (Translate (x + if (mod rawNumber 2) == 0 then  cellOffset else 0) y
   $ Scale 1 1
-- red or black. Red for enlightened position
   $ Color (if (checkerIsChosen state) && (isThereChecker (checkerChosen state) checkers state) && (ifLightened (checkerChosen state) number checkers state) then red else black)
   $ Circle 20)
      : addColoring (stepsLeft - 1) rawNumber (x + (2 * cellOffset)) y checkers state
      where number = (4 * (8 - rawNumber) + (4 - stepsLeft) + 1)


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
