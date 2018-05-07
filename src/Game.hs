module Game where

import qualified Options as Opts
import qualified Selection as Sel
import qualified Table
import qualified Tile
import qualified System.Random as Rand
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Data.Color as Color
import qualified Graphics.Gloss.Data.Picture as Pict
import Graphics.Gloss.Interface.IO.Interact (Event(..), Key(..), SpecialKey(..), KeyState(..), MouseButton(..))

data State = State {status :: GameStatus, gameTable :: Table.Table, selection :: Maybe Sel.Selection}
data GameStatus = Running | Complete -- TODO!

-- entry point
run :: Opts.Options -> IO ()
run opts = do
  gen <- Rand.getStdGen
  Gloss.play window background (Opts.fps opts) (initialState gen) render handleEvent step

-- gloss-starting functions
window :: Gloss.Display
window = Gloss.InWindow "Hexmino" (640, 480) (50,50)

background :: Color.Color
background = Color.white

initialState :: Rand.StdGen -> State
initialState gen = State {status = Complete, gameTable = Table.empty gen, selection = Nothing}

-- rendering functions
render :: State -> Pict.Picture
render st = case status st of
  Running -> Pict.pictures [Table.render $ gameTable st, renderSelection st]
  _ -> Pict.Blank

renderSelection :: State -> Pict.Picture
renderSelection st = case selection st of
  Just sel -> Sel.render sel
  _ -> Pict.Blank

-- event handling / state changing
handleEvent :: Event -> State -> State
handleEvent (EventKey k ks _ pos) = case (k, ks) of
  (SpecialKey KeySpace, Up) -> rotateSelection
  (Char 'n', Up) -> newGame
  (MouseButton LeftButton, Down) -> grabSelection pos
  (MouseButton LeftButton, Up) -> dropSelection pos
  _ -> id
handleEvent (EventMotion pos) = dragSelection pos
handleEvent _ = id

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
newGame state = state {status = Running, gameTable = Table.newGame $ gameTable state}

checkCompleted :: State -> State
checkCompleted state
  | Table.isCompleted table = state {status = Complete, gameTable = Table.clear table}
  | otherwise = state
  where table = gameTable state

-- stepping
step :: Float -> State -> State
step secs st = st {gameTable = Table.step secs $ gameTable st, selection = Sel.step secs <$> selection st}
