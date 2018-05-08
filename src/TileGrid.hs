module TileGrid where

import qualified Tile
import qualified Hex
import qualified Data.Map.Strict as Map
import Data.List (foldl1', foldl', sortOn)
import qualified System.Random as Rand
import qualified Graphics.Gloss.Data.Color as Color
import qualified Graphics.Gloss.Data.Picture as Pict

-- implementation based on https://www.redblobgames.com/grids/hexagons/
data TileGrid = TileGrid {tileMap :: TileMap, range :: Int, tileSize :: Float} deriving Show
type TileMap = Map.Map Axial Tile.Tile
-- coordinate systems - chosen offset is odd-q
newtype Axial = Axial (Int, Int) deriving (Eq, Ord, Show)
newtype Offset = Offset (Int, Int) deriving (Eq, Ord, Show)
newtype Cubic = Cubic (Int, Int, Int) deriving (Eq, Ord, Show)

-- creation functions
empty :: Int -> TileGrid
empty rg = TileGrid {tileMap = Map.empty, range = rg, tileSize = rangeToSize rg}

newGame :: TileGrid -> Rand.StdGen -> (TileGrid, [Tile.Tile], Rand.StdGen)
newGame grid gen = (clear grid, lst, lastGen)
  where
    rg = range grid
    idxedTiles = zip (everyIndex rg) . repeat . Tile.empty $ tileSize grid
    (fullMap, middleGen) = foldl' fillTile (Map.fromList idxedTiles, gen) idxedTiles
    (randInts, lastGen) = randomInts (totalIndexNum rg) middleGen
    lst = map rotateAndTake . sortOn fst . zip randInts $ Map.elems fullMap

fillTile :: (TileMap, Rand.StdGen) -> (Axial, Tile.Tile) -> (TileMap, Rand.StdGen)
fillTile (tMap, gen) (idx, tile) = (Map.insert idx (tile {Tile.faces = (a,b,c)}) tMap, newGen)
  where
    (a, genA) = fillFace (neighVal idx Tile.North tMap, neighVal idx Tile.NorthEast tMap) gen
    (b, genB) = fillFace (neighVal idx Tile.SouthEast tMap, neighVal idx Tile.South tMap) genA
    (c, newGen) = fillFace (neighVal idx Tile.SouthWest tMap, neighVal idx Tile.NorthWest tMap) genB

fillFace :: (Maybe Int, Maybe Int) -> Rand.StdGen -> (Int, Rand.StdGen)
fillFace neigs gen = case neigs of
  (Just 0, Just 0) -> newRand
  (Just n, Just k) -> if n /= 0 then (n, gen) else (k, gen)
  (Nothing, Just n) -> if n /= 0 then (n, gen) else newRand
  (Just n, Nothing) -> if n /= 0 then (n, gen) else newRand
  _ -> (0, gen)
  where newRand = Rand.randomR (1,6) gen

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
render col grid = Pict.pictures . map (renderIndex col grid) . everyIndex $ range grid

renderIndex :: Color.Color -> TileGrid -> Axial ->  Pict.Picture
renderIndex col grid axi = case Map.lookup axi $ tileMap grid of
  Just tile -> Tile.render tile
  _ -> renderEmpty col axi $ tileSize grid

renderEmpty :: Color.Color -> Axial -> Float -> Pict.Picture
renderEmpty col axi rad = Pict.color col $ Hex.render hex
  where hex = Hex.Hexagon {Hex.center = indexCenter axi rad, Hex.radius = rad-1}

-- manipulation functions
putTile :: Tile.Tile -> Axial -> TileGrid -> TileGrid
putTile tile idx grid = grid {tileMap = Map.insert idx fixedTile $ tileMap grid}
  where fixedTile = Tile.moveTo (indexCenter idx (tileSize grid)) tile

grab :: Pict.Point -> TileGrid -> (TileGrid, Maybe Tile.Tile)
grab point grid = case pointToIndex point grid of
  Just idx -> grabIndex idx grid
  _ -> (grid, Nothing)

grabIndex :: Axial -> TileGrid -> (TileGrid, Maybe Tile.Tile)
grabIndex idx grid = case Map.lookup idx $ tileMap grid of
  Just tile -> (grid {tileMap = Map.delete idx $ tileMap grid}, Just tile)
  _ -> (grid, Nothing)

isFull :: TileGrid -> Bool
isFull TileGrid {tileMap = tMap, range = rg} = totalIndexNum rg == Map.size tMap

isCompleted :: TileGrid -> Bool
isCompleted grid
  | isFull grid = all (matchesNeighs (tileMap grid)) . everyIndex $ range grid
  | otherwise = False

clear :: TileGrid -> TileGrid
clear = empty . range

-- utility functions
rangeToSize :: Int -> Float
rangeToSize n = 418 / (2 * (fn+1) + fn)
  where fn = fromIntegral n

indexIsEmpty :: Axial -> TileGrid -> Bool
indexIsEmpty idx = Map.notMember idx . tileMap

totalIndexNum :: Int -> Int
totalIndexNum range = range * (range + 1) `div` 2 * 6 + 1

everyIndex :: Int -> [Axial]
everyIndex = map cubicToAxial . everyCubicIndex

everyCubicIndex :: Int -> [Cubic]
everyCubicIndex range = [Cubic (x,y,z) |
    x <- [(-range)..range],
    y <- [(max (-range) ((-x)-range))..(min range (range-x))],
    let z = (-x)-y,
    x + y + z == 0
  ]

indexCenter :: Axial -> Float -> Pict.Point
indexCenter axi rad = (rad * fromIntegral cl * 1.5, (off + fromIntegral rw) * (-h))
  where
    Offset (cl, rw) = axialToOffset axi
    h = Hex.heightFromRadius rad
    off = if odd cl then 0.5 else 0

pointToIndex :: Pict.Point -> TileGrid -> Maybe Axial
pointToIndex point grid
  | isValidIndex axi grid = Just axi
  | otherwise = Nothing
  where axi = pointToAxial point $ tileSize grid

pointToAxial :: Pict.Point -> Float -> Axial
pointToAxial (x,y) size = Axial (round q, round r)
  where
    q = 2/3 * x / size
    r = ((-1)/3 * x + sqrt 3 / 3 * (-y)) / size

isValidIndex :: Axial -> TileGrid -> Bool
isValidIndex axi grid = cubicDistance (Cubic (0,0,0)) (axialToCubic axi) <= range grid

cubicDistance :: Cubic -> Cubic -> Int
cubicDistance (Cubic (x1,y1,z1)) (Cubic (x2,y2,z2)) = maximum $ map abs [x1-x2, y1-y2, z1-z2]

matchesNeighs :: TileMap -> Axial -> Bool
matchesNeighs tMap idx = case Map.lookup idx tMap of
  Just tile -> all (matchesNeighSide tile idx tMap) Tile.allCardinal
  _ -> False

matchesNeighSide :: Tile.Tile -> Axial -> TileMap -> Tile.Cardinal -> Bool
matchesNeighSide tile idx tMap card = case neighVal idx card tMap of
  Just 0 -> False
  Just n -> Tile.sideValue card tile == n
  Nothing -> True -- always matches an out-of-range tiles

neighVal :: Axial -> Tile.Cardinal -> TileMap -> Maybe Int
neighVal idx card tMap = case neighTile idx card tMap of
  Just tile -> Just $ Tile.sideValue (Tile.opposedCardinal card) tile
  _ -> Nothing

neighTile :: Axial -> Tile.Cardinal -> TileMap -> Maybe Tile.Tile
neighTile idx card = Map.lookup (neighAxial idx card)

neighAxial :: Axial -> Tile.Cardinal -> Axial
neighAxial (Axial (q, r)) card = case card of
  Tile.North -> Axial (q, r-1)
  Tile.NorthEast -> Axial (q+1, r-1)
  Tile.SouthEast -> Axial (q+1, r)
  Tile.South -> Axial (q, r+1)
  Tile.SouthWest -> Axial (q-1, r+1)
  Tile.NorthWest -> Axial (q-1, r)

-- coordinates conversion functions
axialToOffset :: Axial -> Offset
axialToOffset = cubicToOffset . axialToCubic

axialToCubic :: Axial -> Cubic
axialToCubic (Axial (q, r)) = Cubic (q, (-q) - r, r)

cubicToOffset :: Cubic -> Offset
cubicToOffset (Cubic (x, y, z)) = Offset (x, z + (x - (if odd x then 1 else 0)) `div` 2)

cubicToAxial :: Cubic -> Axial
cubicToAxial (Cubic (x, _, z)) = Axial (x, z)
