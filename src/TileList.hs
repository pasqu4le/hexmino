module TileList where

import qualified Tile
import qualified Hex
import qualified Graphics.Gloss.Data.Picture as Pict

-- a stack of tiles and their destination (if they need to move)
type TileList = [(Tile.Tile, Maybe Pict.Point)]

-- creation
empty :: TileList
empty = fromList []

fromList :: [Tile.Tile] -> TileList
fromList tiles = reposition . zip (map (Tile.moveTo (0,spacing*2)) tiles) $ repeat Nothing

-- rendering
render :: TileList -> Pict.Picture
render lst = Pict.pictures . map (Tile.render . fst) $ take 5 lst

--manipulation
reposition :: TileList -> TileList
reposition lst = zip (map fst visible) positions ++ hidden
  where
    (visible, hidden) = splitAt 5 lst
    positions = map Just . zip (repeat 0) $ map (*spacing) [-2..]

putTile :: Tile.Tile -> TileList -> TileList
putTile tile = reposition . ((tile, Nothing) :)

grab :: Pict.Point -> TileList -> (TileList, Maybe Tile.Tile)
grab point lst = (reposition $ zip tls (repeat Nothing) ++ hidden, sel)
    where
      (visible, hidden) = splitAt 5 lst
      (tls, sel) = grabFromVisible point $ map fst visible

grabFromVisible :: Pict.Point -> [Tile.Tile] -> ([Tile.Tile], Maybe Tile.Tile)
grabFromVisible _ [] = ([], Nothing)
grabFromVisible pos (tile:tiles)
  | Tile.contains pos tile = (tiles, Just $ Tile.moveTo pos tile)
  | otherwise = let (lst, res) = grabFromVisible pos tiles in (tile:lst, res)

-- stepping
step :: Float -> TileList -> TileList
step secs lst = map (stepTile secs) visible ++ hidden
  where
    (visible, hidden) = splitAt 5 lst

stepTile :: Float -> (Tile.Tile, Maybe Pict.Point) -> (Tile.Tile, Maybe Pict.Point)
stepTile secs (tile, dest) = case dest of
  Just pos -> moveTile secs tile pos
  Nothing -> (tile, dest)

-- utility functions
moveTile :: Float -> Tile.Tile -> Pict.Point -> (Tile.Tile, Maybe Pict.Point)
moveTile secs tile (x,y)
  | distance <= toCover = (Tile.moveTo (x,y) tile, Nothing)
  | otherwise = (Tile.moveBy (xd * toCover, yd * toCover) tile, Just (x,y))
  where
    (xt, yt) = Hex.center $ Tile.hexagon tile -- tile position
    distance = Hex.pointsDistance (xt, yt) (x,y)
    (xd, yd) = ((x-xt) / distance, (y-yt) / distance) -- direction
    toCover = max 10 (secs * 15 * distance) -- distance to cover

spacing :: Float
spacing = 88
