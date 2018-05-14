module Game where

import qualified Options as Opts
import qualified Selection as Sel
import qualified Table
import qualified Tile
import qualified Widgets as Wid

import Data.Char (isLetter)
import Data.Function (on)
import qualified System.Random as Rand
import qualified System.Exit as Exit
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Data.Color as Color
import qualified Graphics.Gloss.Data.Picture as Pict
import qualified Graphics.Gloss.Data.Point.Arithmetic as PArith
import Graphics.Gloss.Interface.IO.Game (playIO, Event(..), Key(..), SpecialKey(..), KeyState(..), MouseButton(..))

data State = State {
    status :: GameStatus,
    level :: GameLevel,
    duration :: Float,
    topTen :: [String], -- TODO: Scores list
    player :: String,
    gameTable :: Table.Table,
    selection :: Maybe Sel.Selection,
    winScale :: Float
  }
data GameLevel = Beginner | Average | Expert deriving (Show, Eq, Enum, Bounded)
data GameStatus = SplashScreen | Running | Complete | Info

-- scaling from this; NOTE: Game keeps track of window scaling, everything else will assume no scaling
standardSize :: (Int, Int)
standardSize = (700, 480)

scalePoint :: Pict.Point -> Float -> Pict.Point
scalePoint point factor = (1/factor) PArith.* point

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
initialState gen = State {
    status = SplashScreen,
    level = Beginner,
    duration = 0,
    topTen = replicate 10 "PAS 01:12:51 X",
    player = "PAS",
    gameTable = Table.empty gen,
    selection = Nothing,
    winScale = 1}

-- rendering functions
render :: State -> IO Pict.Picture
render st = returnScaled st . (Table.render (gameTable st) :) $ case status st of
  SplashScreen -> [Wid.renderBanner, Wid.renderGameSelector $ level st, Wid.renderTopTen $ topTen st]
  Running -> [Wid.renderTime $ duration st, renderSelection st]
  Complete -> [Wid.renderCompleted $ duration st, Wid.renderNameSelector $ player st, Wid.renderTopTen $ topTen st]
  Info -> [Wid.renderBanner, Wid.renderInfo, Wid.renderTopTen $ topTen st]

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
handleEvent ev st = let se = scaledEvent ev (winScale st) in case status st of
  SplashScreen -> handleSplash se st
  Running -> handleRunning se st
  Complete -> handleComplete se st
  Info -> handleInfo se st

scaledEvent :: Event -> Float ->  Event
scaledEvent ev factor = case ev of
  EventKey k ks mods point -> EventKey k ks mods $ scalePoint point factor
  EventMotion point -> EventMotion $ scalePoint point factor
  _ -> ev  

changeScale :: (Int, Int) -> State -> State
changeScale (w, h) state = state {winScale = newScale}
  where 
    (dw, dh) = standardSize
    newScale = min (floatDiv w dw) (floatDiv h dh)

handleSplash :: Event -> State -> IO State
handleSplash (EventKey k ks _ pos) st = case (k, ks) of
  (SpecialKey KeyEnter, Up) -> return $ newGame st
  (SpecialKey KeyRight, Up) -> return $ toNextLevel st
  (SpecialKey KeyLeft, Up) -> return $ toPreviousLevel st
  _ -> return st
handleSplash _ st = return st

handleInfo :: Event -> State -> IO State
handleInfo (EventKey k ks _ pos) st = case (k, ks) of
  (SpecialKey KeyEnter, Up) -> return $ st {status = SplashScreen}
  _ -> return st 
handleInfo _ st = return st

handleComplete :: Event -> State -> IO State
handleComplete (EventKey k ks m pos) st = case (k, ks) of
  (SpecialKey KeyEnter, Up) -> return $ st {status = SplashScreen}
  (SpecialKey KeyDelete, Up) -> return $ st {player = init $ player st}
  (Char '\x0008', Up) -> handleComplete (EventKey (SpecialKey KeyDelete) Up m pos) st
  (Char c, Up) -> return $ if isLetter c then st {player = take 2 (player st) ++ [c]} else st 
  _ -> return st 
handleComplete _ st = return st

handleRunning :: Event -> State -> IO State
handleRunning (EventKey k ks _ pos) = case (k, ks) of
  (SpecialKey KeySpace, Up) -> return . rotateSelection
  (MouseButton LeftButton, Down) -> return . grabSelection pos
  (MouseButton LeftButton, Up) -> return . dropSelection pos
  _ -> return
handleRunning (EventMotion pos) = return . dragSelection pos
handleRunning _ = return

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
step secs st = return . stepDuration secs $ st {
    gameTable = Table.step secs $ gameTable st,
    selection = Sel.step secs <$> selection st
  }

stepDuration :: Float -> State -> State
stepDuration secs st = case status st of
  Running -> st {duration = secs + duration st}
  _ -> st

-- utils
floatDiv :: Int -> Int -> Float
floatDiv = (/) `on` fromIntegral

levelNum :: State -> Int
levelNum = (1+) . fromEnum . level

toNextLevel :: State -> State
toNextLevel st
  | level st == maxBound = st
  | otherwise = st {level = succ $ level st}

toPreviousLevel :: State -> State
toPreviousLevel st
  | level st == minBound = st
  | otherwise = st {level = pred $ level st}
