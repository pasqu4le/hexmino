module Table where

import qualified Tile
import qualified Hex
import qualified TileGrid as Grid
import qualified TileList
import qualified System.Random as Rand
import qualified Graphics.Gloss.Data.Color as Color
import qualified Graphics.Gloss.Data.Picture as Pict

data Table = Table {tileGrid :: Grid.TileGrid, tileList :: TileList.TileList, randGen :: Rand.StdGen} deriving Show

empty :: Rand.StdGen -> Table
empty gen = Table {tileGrid = Grid.empty 3 38, tileList = TileList.empty, randGen = gen}

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
    Grid.render rugColor $ tileGrid table
  ]

renderListSpace :: Table -> Pict.Picture
renderListSpace table = Pict.pictures [
    Pict.color Color.black $ cutRectangleSolid 220 460,
    Pict.color rugColor $ cutRectangleSolid 210 450,
    Pict.translate 0 175 . Pict.color Color.black $ cutRectangleSolid 200 90,
    TileList.render $ tileList table
  ]

cutRectangleSolid :: Float -> Float -> Pict.Picture
cutRectangleSolid w h = Pict.polygon [(-sw,hh),(-hw,sh),(-hw,-sh),(-sw,-hh),(sw,-hh),(hw,-sh),(hw,sh),(sw,hh)]
  where
    c = w / 10
    hw = w / 2
    hh = h / 2
    sw = hw - c
    sh = hh - c

rugColor :: Color.Color
rugColor = Color.dark $ Color.dark Color.chartreuse

-- manipulation functions
newGame :: Table -> Table
newGame table = table {tileGrid = grid, tileList = TileList.fromList lst, randGen = newGen}
  where (grid, lst, newGen) = Grid.newGame (tileGrid table) $ randGen table

clear :: Table -> Table
clear = empty . randGen

grab :: Pict.Point -> Table -> (Table, Maybe Tile.Tile)
grab (x, y) table = case Grid.grab (x-gridX,y) $ tileGrid table of
  (newGrid, Just sel) -> (table {tileGrid = newGrid}, Just $ Tile.moveBy (gridX, 0) sel)
  _ -> case TileList.grab (x-listX, y) $ tileList table of
    (newList, Just sel) -> (table {tileList = newList}, Just $ Tile.moveBy (listX, 0) sel)
    _ -> (table, Nothing)

putTile :: Tile.Tile -> Pict.Point -> Table -> Table
putTile tile (x,y) table = case Grid.pointToIndex (x-gridX,y) $ tileGrid table of
  Just idx -> putTileInGrid tile idx table
  _ -> putTileInList tile table

putTileInGrid :: Tile.Tile -> Grid.Axial -> Table -> Table
putTileInGrid tile idx table
  | Grid.indexIsEmpty idx $ tileGrid table = table {tileGrid = Grid.putTile tile idx $ tileGrid table}
  | otherwise = putTileInList tile table

putTileInList :: Tile.Tile -> Table -> Table
putTileInList tile table = table {tileList = TileList.putTile (Tile.moveBy (-listX, 0) tile) $ tileList table}

isCompleted :: Table -> Bool
isCompleted = Grid.isCompleted . tileGrid

-- stepping
step :: Float -> Table -> Table
step secs table = table {tileList = TileList.step secs $ tileList table}
