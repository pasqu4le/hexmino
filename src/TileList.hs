module TileList where

import qualified Tile
import qualified Hex

import Control.Lens
import qualified Graphics.Gloss.Data.Picture as Pict
import qualified Graphics.Gloss.Data.Color as Color

-- a stack of tiles and maybe their destination (if they need to move)
data TileList = TileList {
    _tiles :: [Tile.Tile],
    _destinations :: [Maybe Pict.Point],
    _level :: Int
  } deriving Show

-- lenses
tiles :: Lens' TileList [Tile.Tile]
tiles = lens _tiles (\tileList tls -> tileList {_tiles = tls})

destinations :: Lens' TileList [Maybe Pict.Point]
destinations = lens _destinations (\tileList dests -> tileList {_destinations = dests})

level :: Lens' TileList Int
level = lens _level (\tileList lvl -> tileList {_level = lvl})

-- creation
empty :: TileList
empty = fromList 0 []

fromList :: Int -> [Tile.Tile] -> TileList
fromList lvl lst = reposition $ TileList {_tiles = tls, _destinations = [], _level = lvl}
  where tls = map (Tile.moveTo (0, topPos lvl)) lst

-- rendering
render :: TileList -> Pict.Picture
render tLst = case view level tLst of
  0 -> Pict.blank
  lvl -> Pict.pictures . (renderHole lvl :) . map Tile.render $ take (lvl*2) $ view tiles tLst

renderHole :: Int -> Pict.Picture
renderHole lvl = Pict.translate 0 (topPos lvl) . Pict.color Color.black $ Hex.rectangleBlunt 200 (spacing lvl)

--manipulation
reposition :: TileList -> TileList
reposition tLst = tLst & destinations .~ take (lvl * 2) dests
  where 
    lvl = view level tLst
    posMap = (topPos lvl +) . (spacing lvl *) . fromIntegral
    dests = map Just . zip (repeat 0) $ map posMap [1-(lvl*2)..0]

putTile :: Tile.Tile -> TileList -> TileList
putTile tile = reposition . over tiles (tile :)

grab :: Pict.Point -> TileList -> (TileList, Maybe Tile.Tile)
grab point tLst = (reposition $ tLst & tiles .~ (tls ++ hidden), sel)
    where
      (visible, hidden) = splitAt (2 * view level tLst) $ view tiles tLst
      (tls, sel) = grabFromVisible point visible

grabFromVisible :: Pict.Point -> [Tile.Tile] -> ([Tile.Tile], Maybe Tile.Tile)
grabFromVisible _ [] = ([], Nothing)
grabFromVisible pos (tl:tls)
  | Tile.contains pos tl = (tls, Just $ Tile.moveTo pos tl)
  | otherwise = let (lst, res) = grabFromVisible pos tls in (tl:lst, res)

-- stepping
step :: Float -> TileList -> TileList
step secs tLst = tLst & tiles .~ tls & destinations .~ dests
  where
    (visible, hidden) = splitAt (2 * view level tLst) $ view tiles tLst
    movedTiles = zipWith (stepTile secs) visible $ view destinations tLst
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
    (xt, yt) = view Tile.center tile -- tile position
    distance = Hex.pointsDistance (xt, yt) (x,y)
    (xd, yd) = ((x-xt) / distance, (y-yt) / distance) -- direction
    toCover = max 10 (secs * 15 * distance) -- distance to cover

topPos :: Int -> Float
topPos lvl = 220 - spacing lvl / 2

spacing :: Int -> Float
spacing lvl = 440 / (2 * fromIntegral lvl)
