module GraphicsProcessing (gameStart) where

import Types
import Prelude
import WorldProcessing

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Codec.BMP
import System.IO.Unsafe

import DatabaseProcessing (updatePlayer, getTable)

import Database.Persist.Sql

type ScreenXPos = Float
type ScreenYPos = Float

gameStart :: ConnectionPool -> IO ()
gameStart pool = play displayMode bgColor stepsPerSecond initialWorld worldToPicture (eventHandler pool) simStep

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
stepsPerSecond = 2

initialWorld :: WorldObject
initialWorld = (WorldObject createPlayersObject createCheckersObject (State 2 1 1 False) "Choose checker to move")

-- drawing world function
worldToPicture :: WorldObject -> Picture
worldToPicture world = Pictures (worldElements world)

-- calculate coordinate
calculateCoord :: Float -> Float -> Int
calculateCoord x y = (k*4) + (div ((floor x)+205) 100)+1
                      where k = div (150 - (floor y)) 50

calculateState :: Float -> Float -> Int -> Int
calculateState x y lastPos | (x < -350) || (x > 350) = lastPos
                           | (y <= 150) && (y >= 50) = 4
                           | (y <= 0) && (y >= -100) = 5
                           | (y <= -150) && (y >= -250) = 6
                           | otherwise = lastPos

--calculateCoord x y | x < -180 && x >= -130 = 10
  --                  | otherwise = 1

addLetter :: Players -> Char -> Players
addLetter (Players (PlayerData a1 x1 c1 d1) (PlayerData a2 x2 c2 d2)) letter = (Players (PlayerData a1 (x1++(letter:[])) c1 d1) (PlayerData a2 x2 c2 d2))

deleteLast :: [a] -> [a]
deleteLast [] = []
deleteLast (x:[]) = []
deleteLast (x:xs) = x : (deleteLast xs)

deleteLetter :: Players -> Players
deleteLetter (Players (PlayerData a1 x1 c1 d1) (PlayerData a2 x2 c2 d2)) = (Players (PlayerData a1 (deleteLast x1) c1 d1) (PlayerData a2 x2 c2 d2))

-- updatePlayer :: PlayerData -> PlayerData
-- updatePlayer a = a

-- key pressing handling
eventHandler :: ConnectionPool -> Event -> WorldObject -> WorldObject
-- events before game
-- promo screen. Capture any key
-- eventHandler pool pool (EventKey _ _ _ _) (WorldObject players checkers (State 2 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
--  (WorldObject players checkers (State 1 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)
--------------------------------------------------------------------
eventHandler pool (EventKey (Char letter) Down _ _) (WorldObject players checkers (State 2 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject (addLetter players letter) checkers (State 2 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyEnter) Down _ _) (WorldObject (Players x y) checkers (State 2 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject (Players y (updatePlayer x)) checkers (State 3 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyBackspace) Down _ _) (WorldObject players checkers (State 2 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject (deleteLetter players) checkers (State 2 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyDelete) Down _ _) (WorldObject players checkers (State 2 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject (deleteLetter players) checkers (State 2 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)
 ------------------------------------------------------
eventHandler pool (EventKey (Char letter) Down _ _) (WorldObject players checkers (State 3 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject (addLetter players letter) checkers (State 3 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyEnter) Down _ _) (WorldObject (Players x y) checkers (State 3 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject (Players (updatePlayer x) y) checkers (State 4 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyBackspace) Down _ _) (WorldObject players checkers (State 3 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject (deleteLetter players) checkers (State 3 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyDelete) Down _ _) (WorldObject players checkers (State 3 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject (deleteLetter players) checkers (State 3 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)
-- !!!! stats

eventHandler pool (EventKey (SpecialKey KeyEnter) Down _ _) (WorldObject (Players x y) checkers (State 5 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject (Players (updatePlayer x) y) checkers (State 14 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyEnter) Down _ _) (WorldObject (Players x y) checkers (State 7 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject (Players (updatePlayer x) y) checkers (State 14 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyEnter) Down _ _) (WorldObject players checkers (State 4 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State 1 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyEnter) Down _ _) (WorldObject (Players x y) checkers (State 6 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject (Players (updatePlayer x) y) checkers (State (-1) checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyEnter) Down _ _) (WorldObject players checkers (State 8 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State 14 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

  ---------
eventHandler pool (EventKey (SpecialKey KeyEnter) Down _ _) (WorldObject players checkers (State 14 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State 7 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyEnter) Down _ _) (WorldObject players checkers (State 15 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State 8 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyEnter) Down _ _) (WorldObject players checkers (State 16 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State 4 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)
-- up and down
eventHandler pool (EventKey (SpecialKey KeyUp) Down _ _) (WorldObject players checkers (State 4 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State 6 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyUp) Down _ _) (WorldObject players checkers (State 5 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State 4 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyUp) Down _ _) (WorldObject players checkers (State 6 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State 5 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyDown) Down _ _) (WorldObject players checkers (State 5 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State 6 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyDown) Down _ _) (WorldObject players checkers (State 4 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State 5 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyDown) Down _ _) (WorldObject players checkers (State 6 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State 4 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

  -----------------------------------------------------
eventHandler pool (EventKey (SpecialKey KeyUp) Down _ _) (WorldObject players checkers (State 14 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State 16 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyUp) Down _ _) (WorldObject players checkers (State 15 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State 14 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyUp) Down _ _) (WorldObject players checkers (State 16 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State 15 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyDown) Down _ _) (WorldObject players checkers (State 15 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State 16 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyDown) Down _ _) (WorldObject players checkers (State 14 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State 15 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (SpecialKey KeyDown) Down _ _) (WorldObject players checkers (State 16 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State 14 checkerChosen posToMoveChosen checkerIsChosen) alertMessage)
  ----------------------------
-- events during game
-- eventHandler pool (EventKey (SpecialKey KeyUp) Down _ _) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen False) alertMessage) =
 -- (WorldObject players checkers (State playerId ((mod (checkerChosen + 31 - 4) 32) + 1) posToMoveChosen False) alertMessage)

-- eventHandler pool (EventKey (SpecialKey KeyDown) Down _ _) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen False) alertMessage) =
  --(WorldObject players checkers (State playerId ((mod (checkerChosen + 31 + 4) 32) + 1) posToMoveChosen False) alertMessage)

-- eventHandler pool (EventKey (SpecialKey KeyLeft) Down _ _) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen False) alertMessage) =
  -- (WorldObject players checkers (State playerId ((mod (checkerChosen + 31 - 1) 32) + 1) posToMoveChosen False) alertMessage)

-- eventHandler pool (EventKey (SpecialKey KeyRight) Down _ _) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen False) alertMessage) =
  --(WorldObject players checkers (State playerId ((mod (checkerChosen + 31 + 1) 32) + 1) posToMoveChosen False) alertMessage)

--eventHandler pool (EventKey (SpecialKey KeyUp) Down _ _) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen True) alertMessage) =
 -- (WorldObject players checkers (State playerId checkerChosen ((mod (posToMoveChosen + 31 - 4) 32) + 1) True) alertMessage)

-- eventHandler pool (EventKey (SpecialKey KeyDown) Down _ _) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen True) alertMessage) =
 -- (WorldObject players checkers (State playerId checkerChosen ((mod (posToMoveChosen + 31 + 4) 32) + 1) True) alertMessage)

-- eventHandler pool (EventKey (SpecialKey KeyLeft) Down _ _) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen True) alertMessage) =
 -- (WorldObject players checkers (State playerId checkerChosen ((mod (posToMoveChosen + 31 - 1) 32) + 1) True) alertMessage)

-- eventHandler pool (EventKey (SpecialKey KeyRight) Down _ _) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen True) alertMessage) =
 -- (WorldObject players checkers (State playerId checkerChosen ((mod (posToMoveChosen + 31 + 1) 32) + 1) True) alertMessage)

-- Testing the position if there is a checker of the player to move. Game continues according to the result
eventHandler pool (EventKey (SpecialKey KeyEnter) Down _ _) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen False) alertMessage) =
  (WorldObject players checkers (State playerId checkerChosen posToMoveChosen True) "Choose position to move checker to")

-- positions are chosen. The turn starts
eventHandler pool (EventKey (SpecialKey KeyEnter) Down _ _) (WorldObject players checkers (State playerId checkerChosen posToMoveChosen True) alertMessage) =
  gameMove pool (WorldObject players checkers (State playerId checkerChosen posToMoveChosen False) alertMessage)

-- mouse
eventHandler pool (EventKey (MouseButton LeftButton) Down _ (x,y)) (WorldObject players checkers (State 1 checkerChosen posToMoveChosen False) alertMessage)
   | (isThereChecker coord checkers (State 1 checkerChosen posToMoveChosen False)) = (WorldObject players checkers (State 1 coord posToMoveChosen True) alertMessage)
   | otherwise = (WorldObject players checkers (State 1 coord posToMoveChosen False) alertMessage)
   where coord = (calculateCoord x y)

eventHandler pool (EventKey (MouseButton LeftButton) Down _ (x,y)) (WorldObject players checkers (State 1 checkerChosen posToMoveChosen True) alertMessage) =
  gameMove pool (WorldObject players checkers (State 1 checkerChosen (calculateCoord x y) False) alertMessage)

eventHandler pool (EventKey (MouseButton LeftButton) Down _ (x,y)) (WorldObject players checkers (State 0 checkerChosen posToMoveChosen False) alertMessage)
  | (isThereChecker coord checkers (State 0 checkerChosen posToMoveChosen False)) = (WorldObject players checkers (State 0 coord posToMoveChosen True) alertMessage)
  | otherwise = (WorldObject players checkers (State 0 coord posToMoveChosen False) alertMessage)
   where coord = (calculateCoord x y)

eventHandler pool (EventKey (MouseButton LeftButton) Down _ (x,y)) (WorldObject players checkers (State 0 checkerChosen posToMoveChosen True) alertMessage) =
  gameMove pool (WorldObject players checkers (State 0 checkerChosen (calculateCoord x y) False) alertMessage)

----------------------------------------------------------------------------------------------------------------------

eventHandler pool (EventKey (MouseButton LeftButton) Down _ (x,y)) (WorldObject players checkers (State 4 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State (calculateState x y 4) checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (MouseButton LeftButton) Down _ (x,y)) (WorldObject players checkers (State 5 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State (calculateState x y 5) checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (MouseButton LeftButton) Down _ (x,y)) (WorldObject players checkers (State 6 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State (calculateState x y 6) checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

------------------

eventHandler pool (EventKey (MouseButton LeftButton) Down _ (x,y)) (WorldObject players checkers (State 14 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State ((calculateState x y 4) + 10) checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (MouseButton LeftButton) Down _ (x,y)) (WorldObject players checkers (State 15 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State ((calculateState x y 5) + 10) checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

eventHandler pool (EventKey (MouseButton LeftButton) Down _ (x,y)) (WorldObject players checkers (State 16 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
  (WorldObject players checkers (State ((calculateState x y 6) + 10) checkerChosen posToMoveChosen checkerIsChosen) alertMessage)

--eventHandler pool (EventKey (MouseButton LeftButton) Down _ (x,y)) (WorldObject players checkers (State 3 checkerChosen posToMoveChosen checkerIsChosen) alertMessage) =
 -- (WorldObject players checkers (State playerId (calculateCoord x y) posToMoveChosen True) alertMessage)

-- other keys pressed
eventHandler pool _ w = w

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

statsLeftX :: ScreenXPos
statsLeftX = -210

statsMiddleX :: ScreenXPos
statsMiddleX  = 0

statsRightX :: ScreenXPos
statsRightX  = 210

-- collecting pictures to display
worldElements :: WorldObject -> [Picture]
worldElements (WorldObject players checkers state alertMessage) | (stateId state) == 2 = (addStartPage players infobarXOffset infobarYOffset)
                                                                | (stateId state) == 3 = (addStartPageSec players infobarXOffset infobarYOffset)
                                                                | (stateId state) == 4 = (addMenuFst players infobarXOffset infobarYOffset)
                                                                | (stateId state) == 5 = (addMenuSnd players infobarXOffset infobarYOffset)
                                                                | (stateId state) == 6 = (addMenuTrd players infobarXOffset infobarYOffset)
                                                                | (stateId state) == 7 = (addStats players)
                                                                | (stateId state) == 8 = (addBoardStats (getTable))
                                                                | (stateId state) == 14 = (addMenuStatsFst players infobarXOffset infobarYOffset)
                                                                | (stateId state) == 15 = (addMenuStatsSnd players infobarXOffset infobarYOffset)
                                                                | (stateId state) == 16 = (addMenuStatsTrd players infobarXOffset infobarYOffset)
                                                                | (stateId state) == (-1) = (addEndMenu)
                                                                | otherwise = (addBoard boardXOffset boardYOffset checkers state)
                                                                      ++ (addInfobar state players alertMessage infobarXOffset infobarYOffset)

-- menu
----------------------------------------------------------
addStartPage :: Players -> ScreenXPos -> ScreenYPos -> [Picture]
addStartPage  pl_log x y = (pictureBg 0 0):(infobarBg 0 0) : (welcomeText (-200) (45) "Welcome to Checkers! <3"): (startText (-100) 0 "Please, enter your name:") : (startText (-100) (-45) (login (fstPlayerData pl_log))):[]


addStartPageSec :: Players -> ScreenXPos -> ScreenYPos -> [Picture]
addStartPageSec  pl_log x y = (pictureBg 0 0):(infobarBg 0 0) : (welcomeText (-200) (45) "Welcome to Checkers! <3"): (startText (-120) 0 "Please enter your friends name") : (startText (-100) (-45) (login (fstPlayerData pl_log))):[]

addMenuFst :: Players -> ScreenXPos -> ScreenYPos -> [Picture]
addMenuFst player x y = (pictureBg 0 0):(infobarBg x y) : (menuBg 0 100): (chosenText (-200) 100 "Start Game"): (menuBg 0 (-50)): (welcomeText (-200) (-50) "Statistics"): (menuBg 0 (-200)): (welcomeText (-200) (-200) "Exit"): (infobarTitle (x - 50) (y + 20) "Checkers")
  :(infobarText (x - 150) (y - 10) ("Welcome, " ++ (login (sndPlayerData player)) ++ " and " ++ (login (fstPlayerData player))++ "!"))
  :(infobarText (x - 290) (y - 50) "Use keys or the mouse to choose. Press enter to continue"):[]

addMenuSnd :: Players -> ScreenXPos -> ScreenYPos -> [Picture]
addMenuSnd player x y =  (pictureBg 0 0):(infobarBg x y) : (menuBg 0 100): (welcomeText (-200) 100 "Start Game"): (menuBg 0 (-50)): (chosenText (-200) (-50) "Statistics"): (menuBg 0 (-200)): (welcomeText (-200) (-200) "Exit"): (infobarTitle (x - 50) (y + 20) "Checkers")
   :(infobarText (x - 150) (y - 10) ("Welcome, " ++ (login (sndPlayerData player)) ++ " and " ++ (login (fstPlayerData player))++ "!"))
   :(infobarText (x - 290) (y - 50) "Use keys or the mouse to choose. Press enter to continue"):[]

addMenuTrd :: Players -> ScreenXPos -> ScreenYPos -> [Picture]
addMenuTrd player x y =  (pictureBg 0 0):(infobarBg x y) : (menuBg 0 100): (welcomeText (-200) 100 "Start Game"): (menuBg 0 (-50)): (welcomeText (-200) (-50) "Statistics"): (menuBg 0 (-200)): (chosenText (-200) (-200) "Exit"): (infobarTitle (x - 50) (y + 20) "Checkers")
           : (infobarText (x - 150) (y - 10) ("Welcome, " ++ (login (sndPlayerData player)) ++ " and " ++ (login (fstPlayerData player)) ++ "!"))
           :(infobarText (x - 290) (y - 50) "Use keys or the mouse to choose. Press enter to continue"): []

addMenuStatsFst :: Players -> ScreenXPos -> ScreenYPos -> [Picture]
addMenuStatsFst player x y = (pictureBg 0 0):(infobarBg x y) : (menuBg 0 100): (chosenText (-200) 100 "Player statistics"): (menuBg 0 (-50)): (welcomeText (-200) (-50) "Record board"): (menuBg 0 (-200)): (welcomeText (-200) (-200) "Back"): (infobarTitle (x - 50) (y + 20) "Checkers")
  :(infobarText (x - 150) (y - 10) ("Welcome, " ++ (login (sndPlayerData player)) ++ " and " ++ (login (fstPlayerData player))++ "!"))
  :(infobarText (x - 290) (y - 50) "Use keys or the mouse to choose. Press enter to continue"):[]

addMenuStatsSnd :: Players -> ScreenXPos -> ScreenYPos -> [Picture]
addMenuStatsSnd player x y =  (pictureBg 0 0):(infobarBg x y) : (menuBg 0 100): (welcomeText (-200) 100 "Player statistics"): (menuBg 0 (-50)): (chosenText (-200) (-50) "Record board"): (menuBg 0 (-200)): (welcomeText (-200) (-200) "Back"): (infobarTitle (x - 50) (y + 20) "Checkers")
   :(infobarText (x - 150) (y - 10) ("Welcome, " ++ (login (sndPlayerData player)) ++ " and " ++ (login (fstPlayerData player))++ "!"))
   :(infobarText (x - 290) (y - 50) "Use keys or the mouse to choose. Press enter to continue"):[]

addMenuStatsTrd :: Players -> ScreenXPos -> ScreenYPos -> [Picture]
addMenuStatsTrd player x y =  (pictureBg 0 0):(infobarBg x y) : (menuBg 0 100): (welcomeText (-200) 100 "Player statistics"): (menuBg 0 (-50)): (welcomeText (-200) (-50) "Record board"): (menuBg 0 (-200)): (chosenText (-200) (-200) "Back"): (infobarTitle (x - 50) (y + 20) "Checkers")
           : (infobarText (x - 150) (y - 10) ("Welcome, " ++ (login (sndPlayerData player)) ++ " and " ++ (login (fstPlayerData player)) ++ "!"))
           :(infobarText (x - 290) (y - 50) "Use keys or the mouse to choose. Press enter to continue"): []

addEndMenu :: [Picture]
addEndMenu = (pictureBg 0 0):(infobarBg 0 0) : (infobarTitle (-180) 0 "Thanks for playing!!!"):[]

addStats :: Players -> [Picture]
addStats player = (pictureBg 0 0): (statsBgMain statsMiddleX 250) : (welcomeText (-100) 250 "Statistics")
                                  : (statsBg statsLeftX  150) : (statsBg statsMiddleX 150) : (statsBg statsRightX  150) : (welcomeText (statsLeftX-25) 150 "Name")
                                  : (welcomeText (statsMiddleX -50) 150 (login fst_player)) : (welcomeText (statsRightX-50) 150 (login snd_player))
                                  : (statsBg statsLeftX  50)  : (statsBg statsMiddleX 50) : (statsBg statsRightX  50) : (welcomeText (statsLeftX-25) 50 "Wins")
                                  : (welcomeText (statsMiddleX -50) 50 (show (wins stats1)))  : (welcomeText (statsRightX-50) 50 (show (wins stats2)))
                                  : (statsBg statsLeftX (-50)): (statsBg statsMiddleX (-50)) : (statsBg statsRightX  (-50)) : (welcomeText (statsLeftX-25) (-50) "Draws")
                                  : (welcomeText (statsMiddleX -50) (-50) (show (draws stats1))) : (welcomeText (statsRightX-50) (-50) (show (draws stats2)))
                                  : (statsBg statsLeftX  (-150)) : (statsBg statsMiddleX (-150)) : (statsBg statsRightX  (-150)) : (welcomeText (statsLeftX-25) (-150) "Lost")
                                  : (welcomeText (statsMiddleX -50) (-150) (show (losts stats1))) : (welcomeText (statsRightX-50) (-150) (show (losts stats2)))
                                   : (statsBg statsLeftX  (-250)) : (statsBg statsMiddleX (-250)):  (statsBg statsRightX  (-250)): (welcomeText (statsLeftX-70) (-250) "moves")
                                   : (welcomeText (statsMiddleX -50) (-250) (show (moves stats1))) : (welcomeText (statsRightX-50) (-250) (show (moves stats2))):[]
                                  where fst_player = (fstPlayerData player)
                                        snd_player = (sndPlayerData player)
                                        stats1 = (stats fst_player)
                                        stats2 = (stats snd_player)

addBoardStats ::  TopTable -> [Picture]
addBoardStats (TopTable table) = (pictureBg 0 0): (statsBgMain statsMiddleX 250) : (welcomeText (-100) 250 "Record Board")
                                  : (statsBg statsLeftX  150) : (statsBg statsMiddleX 150) : (statsBg statsRightX  150)
                                  : (statsBg statsLeftX  50)  : (statsBg statsMiddleX 50) : (statsBg statsRightX  50)
                                  : (statsBg statsLeftX (-50)): (statsBg statsMiddleX (-50)) : (statsBg statsRightX  (-50))
                                  : (statsBg statsLeftX  (-150)) : (statsBg statsMiddleX (-150)) : (statsBg statsRightX  (-150))
                                   : (statsBg statsLeftX  (-250)) : (statsBg statsMiddleX (-250)):  (statsBg statsRightX  (-250)): (addRecords (TopTable table) 1)
                                  where
makeScreenYPos  :: Float ->  ScreenYPos
makeScreenYPos    x =  (250 - (x * 100.0))


makeTopTable :: TopTable
makeTopTable = (TopTable ((TopTableRecord "Hi" (PlayerStats 1 2 3 4)):(TopTableRecord "Yes" (PlayerStats 1 50 8 1)):(TopTableRecord "No" (PlayerStats 1 48 9 0)):[]))

addOneRecord :: TopTableRecord -> Int -> [Picture]
addOneRecord player y = (welcomeText (statsLeftX-25) (makeScreenYPos (fromIntegral y)) ("#" ++ (show y))) :(welcomeText (statsMiddleX - 50) (makeScreenYPos (fromIntegral y))  (name player))
        : (infobarText (statsRightX-50) ((makeScreenYPos (fromIntegral y)) + 15) ("wins  " ++ (show (wins stats))))
        : (infobarText (statsRightX-50) ((makeScreenYPos (fromIntegral y) - 5)) ("draws  " ++ (show (draws stats))))
        : (infobarText (statsRightX-50) ((makeScreenYPos (fromIntegral y)) - 25) ("losts  " ++ (show (losts stats)))):[]
        where stats = (statsFromTable player)

addRecords ::TopTable -> Int -> [Picture]
addRecords (TopTable []) _ = []
addRecords _ 6 = []
addRecords (TopTable (player:otherPlayers)) numb = (addOneRecord player numb) ++ (addRecords (TopTable otherPlayers) (1 + numb))

menuBg :: ScreenXPos -> ScreenYPos -> Picture
menuBg x y
  = Translate x y
  $ Scale 6 1
  $ unsafePerformIO (loadBMP "data/white.bmp")

statsBg :: ScreenXPos -> ScreenYPos -> Picture
statsBg x y
  = Translate x y
  $ Scale 2 0.8
  $ unsafePerformIO (loadBMP "data/white.bmp")

statsBgMain :: ScreenXPos -> ScreenYPos -> Picture
statsBgMain x y
  = Translate x y
  $ Scale 6 0.8
  $ unsafePerformIO (loadBMP "data/white.bmp")


pictureBg :: ScreenXPos -> ScreenYPos -> Picture
pictureBg x y
  = Translate x y
  $ Scale 0.5 0.5
  $ unsafePerformIO (loadBMP "data/checkersbg.bmp")

welcomeText :: ScreenXPos -> ScreenYPos -> String -> Picture
welcomeText x y message
  = Translate x y
  $ Scale 0.25 0.25
  $ Color black
  $ Text message

chosenText :: ScreenXPos -> ScreenYPos -> String -> Picture
chosenText x y message
  = Translate x y
  $ Scale 0.25 0.25
  $ Color red
  $ Text message

startText :: ScreenXPos -> ScreenYPos -> String -> Picture
startText x y message
  = Translate x y
  $ Scale 0.13 0.13
  $ Color black
  $ Text message


-- collecting infobar elements
addInfobar :: State -> Players -> String -> ScreenXPos -> ScreenYPos -> [Picture]
addInfobar state player message x y
   | (stateId state) == 0 = (infobarBg x y) : (infobarTitle (x - 50) (y + 20) "Checkers") : (infobarText (x - 220) (y - 10) ("move of the player "  ++ (login (fstPlayerData player)))) : (infobarText (x - 220) (y - 50) message) : []
   | otherwise = (infobarBg x y) : (infobarTitle (x - 50) (y + 20) "Checkers") : (infobarText (x - 220) (y - 10) ("move of the player "  ++ (login (sndPlayerData player)))) : (infobarText (x - 220) (y - 50) message) : []
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
addRow n blackNeeded x y checkers state = (addCell 8 blackNeeded x y) ++ (addRow (n - 1) (not blackNeeded) x (y - cellOffset) checkers state) ++ (addCheckers 4 n x y checkers) ++ (addColoring 4 n x y checkers state)

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
   | (checkerIsChosen state) && ((isThereChecker (checkerChosen state) checkers state) && (ifLightened (checkerChosen state) number checkers state) || (number == (checkerChosen state) ))
   = (Translate (x + if (mod rawNumber 2) == 0 then  cellOffset else 0) y
   $ Scale 1 1
-- red or black. Red for enlightened position
   $ Color (makeColorI 128 255 0 160)
   $ ThickCircle  24 10)
      : addColoring (stepsLeft - 1) rawNumber (x + (2 * cellOffset)) y checkers state
   | otherwise = addColoring (stepsLeft - 1) rawNumber (x + (2 * cellOffset)) y checkers state
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
