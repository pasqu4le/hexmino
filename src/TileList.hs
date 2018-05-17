module TileList where

import qualified Tile
import qualified Hex
import qualified Graphics.Gloss.Data.Picture as Pict
import qualified Graphics.Gloss.Data.Color as Color

-- a stack of tiles and maybe their destination (if they need to move)
data TileList = TileList {tiles :: [Tile.Tile], destinations :: [Maybe Pict.Point], level :: Int} deriving Show

-- creation
empty :: TileList
empty = fromList 0 []

fromList :: Int -> [Tile.Tile] -> TileList
fromList lvl lst = reposition $ TileList {tiles = tls, destinations = [], level = lvl}
  where tls = map (Tile.moveTo (0, topPos lvl)) lst

-- rendering
render :: TileList -> Pict.Picture
render tLst = case level tLst of
  0 -> Pict.blank
  lvl -> Pict.pictures . (renderHole lvl :) . map Tile.render $ take (lvl*2) $ tiles tLst

renderHole :: Int -> Pict.Picture
renderHole lvl = Pict.translate 0 (topPos lvl) . Pict.color Color.black $ Hex.rectangleBlunt 200 (spacing lvl)

--manipulation
reposition :: TileList -> TileList
reposition tLst = tLst {destinations = take (lvl * 2) dests}
  where 
    lvl = level tLst
    posMap = (topPos lvl +) . (spacing lvl *) . fromIntegral
    dests = map Just . zip (repeat 0) $ map posMap [1-(lvl*2)..0]

putTile :: Tile.Tile -> TileList -> TileList
putTile tile tLst = reposition $ tLst {tiles = tile : tiles tLst}

grab :: Pict.Point -> TileList -> (TileList, Maybe Tile.Tile)
grab point tLst = (reposition $ tLst {tiles = tls ++ hidden}, sel)
    where
      (visible, hidden) = splitAt (2 * level tLst) $ tiles tLst
      (tls, sel) = grabFromVisible point visible

grabFromVisible :: Pict.Point -> [Tile.Tile] -> ([Tile.Tile], Maybe Tile.Tile)
grabFromVisible _ [] = ([], Nothing)
grabFromVisible pos (tile:tiles)
  | Tile.contains pos tile = (tiles, Just $ Tile.moveTo pos tile)
  | otherwise = let (lst, res) = grabFromVisible pos tiles in (tile:lst, res)

-- stepping
step :: Float -> TileList -> TileList
step secs tLst = tLst {tiles = tls, destinations = dests}
  where
    (visible, hidden) = splitAt (2 * level tLst) $ tiles tLst
    movedTiles = zipWith (stepTile secs) visible $ destinations tLst
    tls = map fst movedTiles ++ hidden
    dests = map snd movedTiles

stepTile :: Float -> Tile.Tile -> Maybe Pict.Point -> (Tile.Tile, Maybe Pict.Point)
stepTile secs tile dest = case dest of
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

topPos :: Int -> Float
topPos lvl = 220 - spacing lvl / 2

spacing :: Int -> Float
spacing lvl = 440 / (2 * fromIntegral lvl)
