module Game where

import qualified Options as Opts
import qualified Selection as Sel
import qualified Table
import qualified Widgets as Wid
import qualified Score

import Control.Lens
import Data.Function (on)
import qualified System.Random as Rand
import qualified System.Exit as Exit
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Data.Color as Color
import qualified Graphics.Gloss.Data.Picture as Pict
import qualified Graphics.Gloss.Data.Point.Arithmetic as PArith
import Graphics.Gloss.Interface.IO.Game (playIO, Event(..), Key(..), SpecialKey(..), KeyState(..), MouseButton(..), Modifiers(..))

data State = State {
    _status :: GameStatus,
    _score :: Score.Score,
    _topTen :: Score.Leaderboard,
    _gameTable :: Table.Table,
    _selection :: Maybe Sel.Selection,
    _winScale :: Float
  }
data GameStatus = SplashScreen | Running | Complete | Info deriving (Show, Eq)

-- lenses
status :: Lens' State GameStatus
status = lens _status (\state st -> state {_status = st})

score :: Lens' State Score.Score
score = lens _score (\state sc -> state {_score = sc})

topTen :: Lens' State Score.Leaderboard
topTen = lens _topTen (\state tt -> state {_topTen = tt})

gameTable :: Lens' State Table.Table
gameTable = lens _gameTable (\state table -> state {_gameTable = table})

selection :: Lens' State (Maybe Sel.Selection)
selection = lens _selection (\state sel -> state {_selection = sel})

winScale :: Lens' State Float
winScale = lens _winScale (\state ws -> state {_winScale = ws})

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
    _status = SplashScreen,
    _score = sc,
    _topTen = leaders,
    _gameTable = Table.empty gen,
    _selection = Nothing,
    _winScale = 1
  }

-- rendering functions
render :: State -> IO Pict.Picture
render st = returnScaled st . (Table.render (view gameTable st) :) $ case view status st of
  SplashScreen -> [Wid.renderBanner, Wid.renderGameSelector $ view score st, Wid.renderTopTen $ view topTen st, Wid.renderInfoButton]
  Running -> [Wid.renderTime $ view score st, renderSelection st, Wid.renderCloseGame]
  Complete -> [Wid.renderCompleted $ view score st, Wid.renderNameSelector $ view score st, Wid.renderTopTen $ view topTen st]
  Info -> [Wid.renderBanner, Wid.renderInfo, Wid.renderTopTen $ view topTen st]

returnScaled :: State -> [Pict.Picture] -> IO Pict.Picture
returnScaled st = return . Pict.scale sc sc . Pict.pictures
  where sc = view winScale st

renderSelection :: State -> Pict.Picture
renderSelection st = case view selection st of
  Just sel -> Sel.render sel
  _ -> Pict.Blank

-- event handling / state changing
handleEvent :: Event -> State -> IO State
handleEvent (EventKey (SpecialKey KeyEsc) Up _ _) _ = Exit.exitSuccess
handleEvent (EventResize newSize) state = return $ changeScale newSize state
handleEvent ev st = let se = scaledEvent ev (view winScale st) in case view status st of
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
changeScale (w, h) state = state & winScale .~ min (floatDiv w dw) (floatDiv h dh)
  where (dw, dh) = standardSize

handleSplash :: Event -> State -> IO State
handleSplash (EventKey k ks _ pos) st = case (k, ks) of
  (SpecialKey KeyEnter, Up) -> return $ newGame st
  (SpecialKey KeyRight, Up) -> return $ st & score %~ Score.toNextLevel
  (SpecialKey KeyLeft, Up) -> return $ st & score %~ Score.toPreviousLevel
  (Char 'i', Up) -> return $ st & status .~ Info 
  (MouseButton LeftButton, Up) -> handleWidgetClick pos st
  _ -> return st
handleSplash _ st = return st

handleInfo :: Event -> State -> IO State
handleInfo (EventKey k ks _ pos) st = case (k, ks) of
  (SpecialKey KeyEnter, Up) -> return $ st & status .~ SplashScreen
  (MouseButton LeftButton, Up) -> handleWidgetClick pos st
  _ -> return st 
handleInfo _ st = return st

handleComplete :: Event -> State -> IO State
handleComplete (EventKey k ks m pos) st = case (k, ks) of
  (SpecialKey KeyEnter, Up) -> submitScore st
  (SpecialKey KeyDelete, Up) -> return $ st & score %~ Score.delFromName
  (Char '\x0008', Up) -> return $ st & score %~ Score.delFromName
  (Char c, Up) -> return $ st & score %~ Score.addToName c
  (MouseButton LeftButton, Up) -> handleWidgetClick pos st
  _ -> return st 
handleComplete _ st = return st

handleWidgetClick :: Pict.Point -> State -> IO State
handleWidgetClick pos st = case Wid.findClicked pos . buttonsInStatus $ view status st of
  Just name -> handleEvent (EventKey (keyFromWidget name) Up noMod (0,0)) st
  _ -> return st

buttonsInStatus :: GameStatus -> [Wid.Name]
buttonsInStatus sta = case sta of
  SplashScreen -> [Wid.NewGame, Wid.LeftArrow, Wid.RightArrow, Wid.Info]
  Running -> [Wid.CloseGame]
  Complete -> [Wid.Delete, Wid.Submit]
  Info -> [Wid.CloseInfo]

keyFromWidget :: Wid.Name -> Key
keyFromWidget name = case name of
  Wid.NewGame -> SpecialKey KeyEnter
  Wid.LeftArrow -> SpecialKey KeyLeft
  Wid.RightArrow -> SpecialKey KeyRight
  Wid.Delete -> SpecialKey KeyDelete
  Wid.Submit -> SpecialKey KeyEnter
  Wid.CloseInfo -> SpecialKey KeyEnter
  Wid.CloseGame -> Char '\x0008'
  Wid.Info ->  Char 'i'
  _ -> SpecialKey KeyEsc -- something went wrong here

submitScore :: State -> IO State
submitScore st = do 
  leaders <- Score.submit $ view score st
  return $ st & status .~ SplashScreen & topTen .~ leaders

handleRunning :: Event -> State -> IO State
handleRunning (EventKey k ks _ pos) = case (k, ks) of
  (SpecialKey KeySpace, Up) -> return . rotateSelection
  (Char '\x0008', Up) -> return . backToSpashScreen
  (MouseButton LeftButton, Down) -> return . grabSelection pos
  (MouseButton LeftButton, Up) -> handleRunningClick pos
  _ -> return
handleRunning (EventMotion pos) = return . dragSelection pos
handleRunning _ = return

backToSpashScreen :: State -> State
backToSpashScreen st = st & status .~ SplashScreen & gameTable %~ Table.clear

handleRunningClick :: Pict.Point -> State -> IO State
handleRunningClick pos st = case view selection st of
  Just sel -> return $ dropSelection pos st
  _ -> handleWidgetClick pos st

rotateSelection :: State -> State
rotateSelection st = st & selection %~ (Sel.rotate <$>)

dragSelection :: Pict.Point -> State -> State
dragSelection pos st = st & selection %~ (Sel.moveTo pos <$>)

grabSelection :: Pict.Point -> State -> State
grabSelection pos st = st & gameTable .~ newTable & selection .~ (Sel.make <$> newTile)
  where (newTable, newTile) = Table.grab pos $ view gameTable st

dropSelection :: Pict.Point -> State -> State
dropSelection point st = case view selection st of
  Just sel -> checkCompleted $ st & gameTable %~ Table.putTile (view Sel.tile sel) point & selection .~ Nothing
  _ -> st

newGame :: State -> State
newGame state = state
  & status .~ Running 
  & gameTable %~ Table.newGame (Score.levelNum $ view score state)
  & score %~ Score.clearTime

checkCompleted :: State -> State
checkCompleted state
  | Table.isCompleted $ view gameTable state = state & status .~ Complete & gameTable %~ Table.clear
  | otherwise = state

-- stepping
step :: Float -> State -> IO State
step secs state = return $ state
  & gameTable %~ Table.step secs
  & selection %~ (Sel.step secs <$>)
  & score %~ stepTime
  where stepTime = if view status state == Running then Score.step secs else id

-- utils
floatDiv :: Int -> Int -> Float
floatDiv = (/) `on` fromIntegral

noMod :: Modifiers
noMod = Modifiers {shift = Up, ctrl = Up, alt = Up}
