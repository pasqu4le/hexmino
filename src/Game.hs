module Game where

import qualified Options as Opts
import qualified Table
import qualified Tile
import qualified System.Random as Rand
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Data.Color as Color
import qualified Graphics.Gloss.Data.Picture as Pict
import Graphics.Gloss.Interface.IO.Interact (Event(..), Key(..), SpecialKey(..), KeyState(..), MouseButton(..))

data State = State {status :: GameStatus, gameTable :: Table.Table, selection :: Maybe Tile.Tile}
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
  Just tile -> Tile.render tile
  _ -> Pict.Blank

-- event handling / state changing
handleEvent :: Event -> State -> State
handleEvent (EventKey k ks _ pos) = case (k, ks) of
  (SpecialKey KeySpace, Up) -> rotateSelection
  (Char 'n', Up) -> newGame
  (MouseButton LeftButton, Down) -> grabTile pos
  (MouseButton LeftButton, Up) -> dropTile pos
  _ -> id
handleEvent (EventMotion pos) = dragTile pos
handleEvent _ = id

rotateSelection :: State -> State
rotateSelection st = case selection st of
  Just tile -> st {selection = Just $ Tile.rotate tile}
  _ -> st

dragTile :: Pict.Point -> State -> State
dragTile pos st = case selection st of
  Just tile -> st {selection = Just $ Tile.moveTo pos tile}
  _ -> st

grabTile :: Pict.Point -> State -> State
grabTile pos st = st {gameTable = newTable, selection = newSel}
  where (newTable, newSel) = Table.grab pos $ gameTable st

dropTile :: Pict.Point -> State -> State
dropTile point st = case selection st of
  Just tile -> checkCompleted $ st {gameTable = Table.putTile tile point $ gameTable st, selection = Nothing}
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
step secs state = state {gameTable = Table.step secs $ gameTable state}
