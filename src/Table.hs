module Table where

import qualified Tile
import qualified Hex
import qualified TileGrid as Grid
import qualified TileList

import Control.Lens
import qualified System.Random as Rand
import qualified Graphics.Gloss.Data.Color as Color
import qualified Graphics.Gloss.Data.Picture as Pict

data Table = Table {
    _tileGrid :: Grid.TileGrid,
    _tileList :: TileList.TileList,
    _randGen :: Rand.StdGen
  } deriving Show

-- lenses
tileGrid :: Lens' Table Grid.TileGrid
tileGrid = lens _tileGrid (\table grid -> table {_tileGrid = grid})

tileList :: Lens' Table TileList.TileList
tileList = lens _tileList (\table lst -> table {_tileList = lst})

randGen :: Lens' Table Rand.StdGen
randGen = lens _randGen (\table gen -> table {_randGen = gen})

-- creation
empty :: Rand.StdGen -> Table
empty gen = Table {_tileGrid = Grid.empty, _tileList = TileList.empty, _randGen = gen}

-- displacements; NOTE: Table keeps track of the grid and list displacement, so both can assume they are centered
gridX, listX :: Float
gridX = -120
listX = 230

-- rendering functions
render :: Table -> Pict.Picture
render table = Pict.pictures [
    Pict.translate gridX 0 $ renderGridSpace table,
    Pict.translate listX 0 $ renderListSpace table
  ]

renderGridSpace :: Table -> Pict.Picture
renderGridSpace table = Pict.pictures [
    Pict.color Color.black $ Hex.hexagonSolidPointy 240,
    Grid.render rugColor $ view tileGrid table
  ]

renderListSpace :: Table -> Pict.Picture
renderListSpace table = Pict.pictures [
    Pict.color Color.black $ Hex.rectangleBlunt 220 460,
    Pict.color rugColor $ Hex.rectangleBlunt 210 450,
    TileList.render $ view tileList table
  ]

rugColor :: Color.Color
rugColor = Color.dark $ Color.dark Color.chartreuse

-- manipulation functions
newGame :: Int -> Table -> Table
newGame level table = table & tileGrid .~ grid & tileList .~ TileList.fromList level lst & randGen .~ newGen
  where (grid, lst, newGen) = Grid.newGame level $ view randGen table

clear :: Table -> Table
clear = empty . view randGen

grab :: Pict.Point -> Table -> (Table, Maybe Tile.Tile)
grab (x, y) table = case Grid.grab (x-gridX,y) $ view tileGrid table of
  (newGrid, Just sel) -> (table & tileGrid .~ newGrid, Just $ Tile.moveBy (gridX, 0) sel)
  _ -> case TileList.grab (x-listX, y) $ view tileList table of
    (newList, Just sel) -> (table & tileList .~ newList, Just $ Tile.moveBy (listX, 0) sel)
    _ -> (table, Nothing)

putTile :: Tile.Tile -> Pict.Point -> Table -> Table
putTile tile (x,y) table = case Grid.pointToIndex (x-gridX,y) $ view tileGrid table of
  Just idx -> putTileInGrid tile idx table
  _ -> putTileInList tile table

putTileInGrid :: Tile.Tile -> Grid.Index -> Table -> Table
putTileInGrid tile idx table
  | Grid.indexIsEmpty idx $ view tileGrid table = table & tileGrid %~ Grid.putTile tile idx
  | otherwise = putTileInList tile table

putTileInList :: Tile.Tile -> Table -> Table
putTileInList tile table = table & tileList %~ TileList.putTile (Tile.moveBy (-listX, 0) tile)

isCompleted :: Table -> Bool
isCompleted = Grid.isCompleted . view tileGrid

-- stepping
step :: Float -> Table -> Table
step secs = over tileList (TileList.step secs)
