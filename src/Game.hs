module Game where

import qualified Options as Opts
import qualified Selection as Sel
import qualified Table
import qualified Tile

import Data.Function (on)
import qualified System.Random as Rand
import qualified System.Exit as Exit
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Data.Color as Color
import qualified Graphics.Gloss.Data.Picture as Pict
import Graphics.Gloss.Interface.IO.Game (playIO, Event(..), Key(..), SpecialKey(..), KeyState(..), MouseButton(..))

data State = State {status :: GameStatus, level :: GameLevel, gameTable :: Table.Table, selection :: Maybe Sel.Selection, winScale :: Float}
data GameLevel = Beginner | Average | Expert deriving (Show, Enum, Bounded)
data GameStatus = SplashScreen | SelectLevel | Running | Complete | Info

-- scaling from this; NOTE: Game keeps track of window scaling, everything else will assume no scaling
standardSize :: (Int, Int)
standardSize = (700, 480)

scalePoint :: Pict.Point -> Float -> Pict.Point
scalePoint (x, y) factor = (x / factor, y / factor)

-- entry point
run :: Opts.Options -> IO ()
run opts = do
  gen <- Rand.getStdGen
  playIO window background (Opts.fps opts) (initialState gen) render handleEvent step

-- gloss-starting functions
window :: Gloss.Display
window = Gloss.InWindow "Hexmino" standardSize (50,50)

background :: Color.Color
background = Color.white

initialState :: Rand.StdGen -> State
initialState gen = State {status = SplashScreen, level = Average, gameTable = Table.empty gen, selection = Nothing, winScale = 1}

-- rendering functions
render :: State -> IO Pict.Picture
render st = returnScaled st . (Table.render (gameTable st) :) $ case status st of
  Running -> [renderSelection st]
  _ -> []

returnScaled :: State -> [Pict.Picture] -> IO Pict.Picture
returnScaled st = return . Pict.scale (winScale st) (winScale st) . Pict.pictures

renderSelection :: State -> Pict.Picture
renderSelection st = case selection st of
  Just sel -> Sel.render sel
  _ -> Pict.Blank

-- event handling / state changing
handleEvent :: Event -> State -> IO State
handleEvent (EventKey (SpecialKey KeyEsc) Up _ _) _ = Exit.exitSuccess
handleEvent (EventResize newSize) state = return $ changeScale newSize state
handleEvent ev state = case status state of
  SplashScreen -> handleSplash ev state
  SelectLevel -> handleSelect ev state
  Running -> handleRunning ev state
  Complete -> handleComplete ev state
  Info -> handleInfo ev state

changeScale :: (Int, Int) -> State -> State
changeScale (w, h) state = state {winScale = newScale}
  where 
    (dw, dh) = standardSize
    newScale = min (floatDiv w dw) (floatDiv h dh)

handleSplash :: Event -> State -> IO State
handleSplash (EventKey k ks _ pos) st = case (k, ks) of
  (SpecialKey KeyEnter, Up) -> return $ st {status = SelectLevel}
  _ -> return st
handleSplash _ st = return st

handleSelect :: Event -> State -> IO State
handleSelect (EventKey k ks _ pos) = case (k, ks) of
  (SpecialKey KeyEnter, Up) -> return . newGame
  _ -> return
handleSelect _ = return

handleInfo :: Event -> State -> IO State
handleInfo (EventKey k ks _ pos) st = case (k, ks) of
  (SpecialKey KeyEnter, Up) -> return $ st {status = SplashScreen}
  _ -> return st 
handleInfo _ st = return st

handleComplete :: Event -> State -> IO State
handleComplete (EventKey k ks _ pos) st = case (k, ks) of
  (SpecialKey KeyEnter, Up) -> return $ st {status = SelectLevel}
  _ -> return st 
handleComplete _ st = return st

handleRunning :: Event -> State -> IO State
handleRunning (EventKey k ks _ pos) = case (k, ks) of
  (SpecialKey KeySpace, Up) -> return . rotateSelection
  (MouseButton LeftButton, Down) -> withScaledPoint grabSelection pos
  (MouseButton LeftButton, Up) -> withScaledPoint dropSelection pos
  _ -> return
handleRunning (EventMotion pos) = withScaledPoint dragSelection pos
handleRunning _ = return

withScaledPoint :: (Pict.Point -> State -> State) -> Pict.Point -> State -> IO State
withScaledPoint func point state = return $ func scPoint state
  where scPoint = scalePoint point $ winScale state

rotateSelection :: State -> State
rotateSelection st = st {selection = Sel.rotate <$> selection st}

dragSelection :: Pict.Point -> State -> State
dragSelection pos st = st {selection = Sel.moveTo pos <$> selection st}

grabSelection :: Pict.Point -> State -> State
grabSelection pos st = st {gameTable = newTable, selection = Sel.make <$> newTile}
  where (newTable, newTile) = Table.grab pos $ gameTable st

dropSelection :: Pict.Point -> State -> State
dropSelection point st = case selection st of
  Just sel -> checkCompleted $ st {gameTable = Table.putTile (Sel.tile sel) point $ gameTable st, selection = Nothing}
  _ -> st

newGame :: State -> State
newGame state = state {status = Running, gameTable = Table.newGame (levelNum state) $ gameTable state}

checkCompleted :: State -> State
checkCompleted state
  | Table.isCompleted table = state {status = Complete, gameTable = Table.clear table}
  | otherwise = state
  where table = gameTable state

-- stepping
step :: Float -> State -> IO State
step secs st = return $ st {gameTable = Table.step secs $ gameTable st, selection = Sel.step secs <$> selection st}

-- utils
floatDiv :: Int -> Int -> Float
floatDiv = (/) `on` fromIntegral

levelNum :: State -> Int
levelNum = (1+) . fromEnum . level