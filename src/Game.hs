module Game where

import qualified Options as Opts
import qualified Selection as Sel
import qualified Table
import qualified Tile
import qualified Widgets as Wid
import qualified Score

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
    score :: Score.Score,
    topTen :: Score.Leaderboard,
    gameTable :: Table.Table,
    selection :: Maybe Sel.Selection,
    winScale :: Float
  }
data GameStatus = SplashScreen | Running | Complete | Info deriving (Show, Eq)

-- scaling from this; NOTE: Game keeps track of window scaling, everything else will assume no scaling
standardSize :: (Int, Int)
standardSize = (700, 480)

scalePoint :: Pict.Point -> Float -> Pict.Point
scalePoint point factor = (1/factor) PArith.* point

-- entry point
run :: Opts.Options -> IO ()
run opts = do
  gen <- Rand.getStdGen
  state <- initialState gen
  playIO window background (Opts.fps opts) state render handleEvent step

-- gloss-starting functions
window :: Gloss.Display
window = Gloss.InWindow "Hexmino" standardSize (50,50)

background :: Color.Color
background = Color.white

initialState :: Rand.StdGen -> IO State
initialState gen = do
  sc <- Score.readPlayer
  leaders <- Score.readTopTen
  return $ State {
    status = SplashScreen,
    score = sc,
    topTen = leaders,
    gameTable = Table.empty gen,
    selection = Nothing,
    winScale = 1
  }

-- rendering functions
render :: State -> IO Pict.Picture
render st = returnScaled st . (Table.render (gameTable st) :) $ case status st of
  SplashScreen -> [Wid.renderBanner, Wid.renderGameSelector $ score st, Wid.renderTopTen $ topTen st, Wid.renderInfoButton]
  Running -> [Wid.renderTime $ score st, renderSelection st, Wid.renderCloseGame]
  Complete -> [Wid.renderCompleted $ score st, Wid.renderNameSelector $ score st, Wid.renderTopTen $ topTen st, Wid.renderInfoButton]
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
  (SpecialKey KeyRight, Up) -> return $ st {score = Score.toNextLevel $ score st}
  (SpecialKey KeyLeft, Up) -> return $ st {score = Score.toPreviousLevel $ score st}
  (MouseButton LeftButton, Up) -> handleWidgetClick pos [Wid.NewGame, Wid.LeftArrow, Wid.RightArrow, Wid.Info] st
  _ -> return st
handleSplash _ st = return st

handleInfo :: Event -> State -> IO State
handleInfo (EventKey k ks _ pos) st = case (k, ks) of
  (SpecialKey KeyEnter, Up) -> return $ st {status = SplashScreen}
  (MouseButton LeftButton, Up) -> handleWidgetClick pos [Wid.CloseInfo] st
  _ -> return st 
handleInfo _ st = return st

handleComplete :: Event -> State -> IO State
handleComplete (EventKey k ks m pos) st = case (k, ks) of
  (SpecialKey KeyEnter, Up) -> submitScore st
  (SpecialKey KeyDelete, Up) -> return $ st {score = Score.delFromName $ score st}
  (Char '\x0008', Up) -> return $ st {score = Score.delFromName $ score st}
  (Char c, Up) -> return $ st {score = Score.addToName c $ score st}
  (MouseButton LeftButton, Up) -> handleWidgetClick pos [Wid.Delete, Wid.Submit] st
  _ -> return st 
handleComplete _ st = return st

handleWidgetClick :: Pict.Point -> [Wid.Name] -> State -> IO State
handleWidgetClick pos names st = case Wid.findClicked pos names of
  Just Wid.NewGame -> return $ newGame st
  Just Wid.LeftArrow -> return $ st {score = Score.toPreviousLevel $ score st}
  Just Wid.RightArrow -> return $ st {score = Score.toNextLevel $ score st}
  Just Wid.Delete -> return $ st {score = Score.delFromName $ score st}
  Just Wid.Submit -> submitScore st
  Just Wid.CloseInfo -> return $ st {status = SplashScreen}
  Just Wid.CloseGame -> return $ st {status = SplashScreen, gameTable = Table.clear $ gameTable st}
  Just Wid.Info -> return $ st {status = Info}
  _ -> return st

submitScore :: State -> IO State
submitScore st = do 
  leaders <- Score.submit $ score st
  return $ st {status = SplashScreen, topTen = leaders}

handleRunning :: Event -> State -> IO State
handleRunning (EventKey k ks _ pos) = case (k, ks) of
  (SpecialKey KeySpace, Up) -> return . rotateSelection
  (MouseButton LeftButton, Down) -> return . grabSelection pos
  (MouseButton LeftButton, Up) -> handleRunningClick pos
  _ -> return
handleRunning (EventMotion pos) = return . dragSelection pos
handleRunning _ = return

handleRunningClick :: Pict.Point -> State -> IO State
handleRunningClick pos st = case selection st of
  Just sel -> return $ dropSelection pos st
  _ -> handleWidgetClick pos [Wid.CloseGame] st

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
newGame state = state {
    status = Running, 
    gameTable = Table.newGame (Score.levelNum $ score state) $ gameTable state,
    score = Score.clearTime $ score state
  }

checkCompleted :: State -> State
checkCompleted state
  | Table.isCompleted table = state {status = Complete, gameTable = Table.clear table}
  | otherwise = state
  where table = gameTable state

-- stepping
step :: Float -> State -> IO State
step secs st = return $ st {
    gameTable = Table.step secs $ gameTable st,
    selection = Sel.step secs <$> selection st,
    score = stepTime $ score st
  }
  where stepTime = if status st == Running then Score.step secs else id

-- utils
floatDiv :: Int -> Int -> Float
floatDiv = (/) `on` fromIntegral
