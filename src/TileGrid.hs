module TileGrid where

import qualified Tile
import qualified Hex
import qualified Data.Map.Strict as Map
import Data.List (foldl', sortOn)
import qualified System.Random as Rand
import qualified Graphics.Gloss.Data.Color as Color
import qualified Graphics.Gloss.Data.Picture as Pict
import qualified Math.Geometry.Grid as Grid
import qualified Math.Geometry.Grid.Hexagonal2 as HexGrid
import qualified Math.Geometry.GridMap as GridMap (toGrid, toMap, lookup, insert, delete, toList, adjust, elems)
import qualified Math.Geometry.GridMap.Lazy as LGridMap
import Math.Geometry.Grid.HexagonalInternal2 (HexDirection(..))

-- implementation was guided by https://www.redblobgames.com/grids/hexagons/
-- uses grid package, for reference: flat top with Axial (x,y) coordinates
type TileGrid = LGridMap.LGridMap HexGrid.HexHexGrid Tile.Tile
type Index = Grid.Index HexGrid.HexHexGrid

-- creation functions
empty :: TileGrid
empty = LGridMap.empty $ HexGrid.hexHexGrid 0

newGame :: Int -> Rand.StdGen -> (TileGrid, [Tile.Tile], Rand.StdGen)
newGame level gen = (emptyGrid, lst, lastGen)
  where
    emptyGrid = LGridMap.empty $ HexGrid.hexHexGrid (level + 1)
    (fullGrid, middleGen) = foldl' matchSides (emptyGrid, gen) $ Grid.edges emptyGrid
    (randInts, lastGen) = randomInts (indicesNum emptyGrid) middleGen
    lst = map rotateAndTake . sortOn fst . zip randInts $ GridMap.elems fullGrid

matchSides :: (TileGrid, Rand.StdGen) -> (Index, Index) -> (TileGrid, Rand.StdGen)
matchSides (tileGrid, gen) (idxA, idxB) = case (sideA, sideB) of
  (Nothing, _) -> matchSides (putTile newTile idxA tileGrid, gen) (idxA, idxB)
  (_, Nothing) -> matchSides (putTile newTile idxB tileGrid, gen) (idxA, idxB)
  (Just 0, Just 0) -> (adjustSide dirA newVal idxA $ adjustSide dirB newVal idxB tileGrid, newGen)
  (Just n, Just k) -> ((if n /= 0 then adjustSide dirB n idxB else adjustSide dirA k idxA) tileGrid, gen)
  where
    dirA = head $ Grid.directionTo (GridMap.toGrid tileGrid) idxA idxB
    dirB = oppositeDirection dirA
    sideA = Tile.sideValue dirA <$> GridMap.lookup idxA tileGrid
    sideB = Tile.sideValue dirB <$> GridMap.lookup idxB tileGrid
    (newVal, newGen) = Rand.randomR (1,6) gen
    newTile = Tile.empty $ tileSize tileGrid

adjustSide :: HexDirection -> Int -> Index -> TileGrid -> TileGrid
adjustSide dir val idx = GridMap.adjust (Tile.changeSide dir val) idx

randomInts :: Int -> Rand.StdGen -> ([Int], Rand.StdGen)
randomInts 0 gen = ([], gen)
randomInts n gen = (val : follRand, follGen)
  where
    (val, nextGen) = Rand.random gen
    (follRand, follGen) = randomInts (n-1) nextGen

rotateAndTake :: (Int, Tile.Tile) -> Tile.Tile
rotateAndTake (n, tile) = (!! mod n 3) $ iterate Tile.rotate tile

-- rendering functions
render :: Color.Color -> TileGrid -> Pict.Picture
render col tileGrid
  | isEmpty tileGrid = Pict.color col $ Hex.hexagonSolidPointy 230
  | otherwise = Pict.pictures . map (renderIndex col tileGrid) $ indices tileGrid

renderIndex :: Color.Color -> TileGrid -> Index ->  Pict.Picture
renderIndex col tileGrid idx = case GridMap.lookup idx tileGrid of
  Just tile -> Tile.render tile
  _ -> renderEmpty col idx $ tileSize tileGrid

renderEmpty :: Color.Color -> Index -> Float -> Pict.Picture
renderEmpty col idx rad = Pict.color col $ Hex.render hex
  where hex = Hex.Hexagon {Hex.center = indexCenter idx rad, Hex.radius = rad-1}

-- manipulation functions
putTile :: Tile.Tile -> Index -> TileGrid -> TileGrid
putTile tile idx tileGrid = GridMap.insert idx fixedTile tileGrid
  where fixedTile = Tile.moveTo (indexCenter idx (tileSize tileGrid)) tile

grab :: Pict.Point -> TileGrid -> (TileGrid, Maybe Tile.Tile)
grab point tileGrid = case pointToIndex point tileGrid of
  Just idx -> grabIndex idx tileGrid
  _ -> (tileGrid, Nothing)

grabIndex :: Index -> TileGrid -> (TileGrid, Maybe Tile.Tile)
grabIndex idx tileGrid = case GridMap.lookup idx tileGrid of
  Just tile -> (GridMap.delete idx tileGrid, Just tile)
  _ -> (tileGrid, Nothing)

isFull :: TileGrid -> Bool
isFull tileGrid = indicesNum tileGrid == length (GridMap.toList tileGrid)

isCompleted :: TileGrid -> Bool
isCompleted tileGrid
  | isFull tileGrid = all (matchingSides tileGrid) . Grid.edges $ GridMap.toGrid tileGrid
  | otherwise = False

-- utility functions
isEmpty :: TileGrid -> Bool
isEmpty = Grid.null . GridMap.toGrid

indexIsEmpty :: Index -> TileGrid -> Bool
indexIsEmpty idx = Map.notMember idx . GridMap.toMap

indicesNum :: TileGrid -> Int
indicesNum = Grid.tileCount . GridMap.toGrid

indices :: TileGrid -> [Index]
indices = Grid.indices . GridMap.toGrid

indexCenter :: Index -> Float -> Pict.Point -- NOTE: based on odd-q
indexCenter (x, y) radius = (radius * column * 1.5, (offset + row) * (-height))
  where
    column = fromIntegral x
    row = fromIntegral $ (-x) - y + (x - (if odd x then 1 else 0)) `div` 2
    height = Hex.heightFromRadius radius
    offset = if odd x then 0.5 else 0

pointToIndex :: Pict.Point -> TileGrid -> Maybe Index
pointToIndex (x, y) tileGrid
  | Grid.contains tileGrid idx = Just idx
  | otherwise = Nothing
  where
    size = tileSize tileGrid
    idxX = 2/3 * x / size
    idxZ = ((-1)/3 * x + sqrt 3 / 3 * (-y)) / size
    idxY = (-idxX) - idxZ
    idx = (round idxX, round idxY)

matchingSides :: TileGrid -> (Index, Index) -> Bool
matchingSides tileGrid (idxA, idxB) = case (look idxA, look idxB) of
  (Just tileA, Just tileB) -> Tile.sideValue dirA tileA == Tile.sideValue dirB tileB
  _ -> False
  where 
    look = (`GridMap.lookup` tileGrid)
    dirA = head $ Grid.directionTo (GridMap.toGrid tileGrid) idxA idxB
    dirB = oppositeDirection dirA

tileSize :: TileGrid -> Float
tileSize tileGrid = 418 / (2 * rg  + (rg-1))
  where rg = fromIntegral . Grid.size $ GridMap.toGrid tileGrid

oppositeDirection :: HexDirection -> HexDirection
oppositeDirection car = case car of
  North -> South
  Northeast -> Southwest
  Southeast -> Northwest
  South -> North
  Southwest -> Northeast
  Northwest -> Southeast