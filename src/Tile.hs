module Tile where

import qualified Hex

import Control.Lens
import qualified Graphics.Gloss.Data.Color as Color
import qualified Graphics.Gloss.Data.Picture as Pict
import Math.Geometry.Grid.HexagonalInternal2 (HexDirection(..))

data Tile = Tile {
    _hexagon :: Hex.Hexagon,
    _faces :: (Int, Int, Int)
  } deriving (Show)

empty :: Float -> Tile
empty size = Tile (Hex.make (0,0) size) (0,0,0)

-- lenses
hexagon :: Lens' Tile Hex.Hexagon
hexagon = lens _hexagon (\tile hex -> tile {_hexagon = hex})

faces :: Lens' Tile (Int, Int, Int)
faces = lens _faces (\tile fcs -> tile {_faces = fcs})

center :: Lens' Tile Pict.Point
center = hexagon . Hex.center

radius :: Lens' Tile Float
radius = hexagon . Hex.radius

-- rendering functions
render :: Tile -> Pict.Picture
render tile = Pict.translate x y $ renderCentered tile
  where (x, y) = view center tile

renderRotated :: Float -> Tile -> Pict.Picture
renderRotated rot tile = Pict.translate x y . Pict.rotate rot $ renderCentered tile
  where (x, y) = view center tile

renderCentered :: Tile -> Pict.Picture
renderCentered tile = Pict.pictures [
    Pict.color Color.white . Hex.renderCentered $ view hexagon tile,
    Pict.color Color.black $ renderLines tile,
    Pict.color Color.black $ renderFaces tile
  ]

renderLines :: Tile -> Pict.Picture
renderLines tile = Pict.pictures lns
  where
    (_: border) = take 4 . Hex.hexagonPath $ view radius tile
    faceLn = Pict.line ((0,0):border)
    lns = zipWith Pict.rotate facesRotations $ replicate 3 faceLn

renderFaces :: Tile -> Pict.Picture
renderFaces tile = Pict.pictures fcs
  where
    (a, b, c) = view faces tile
    fcs = zipWith Pict.rotate facesRotations $ map (renderFace (view radius tile)) [a,b,c]

renderFace :: Float -> Int -> Pict.Picture
renderFace r n = case n of
  6 -> Pict.pictures [Pict.rotate (-15) far, Pict.rotate 15 far, Pict.rotate (-30) near, Pict.rotate 30 near, renderFace r 2]
  5 -> Pict.pictures [Pict.rotate (-30) near, Pict.rotate 30 near, renderFace r 3]
  4 -> Pict.pictures [near, renderFace r 3]
  3 -> Pict.pictures [far, renderFace r 2]
  2 -> Pict.pictures [Pict.rotate (-40) far, Pict.rotate 40 far]
  1 -> far
  _ -> Pict.Blank
  where
    c = Pict.circleSolid (r/10)
    h = Hex.heightFromRadius r
    far = Pict.translate (r/3) (h/3) c
    near = Pict.translate (r/6) (h/6) c

-- manupulation functions
rotate :: Tile -> Tile
rotate tile = tile & faces .~ (c,a,b)
  where (a,b,c) = view faces tile

moveTo :: Pict.Point -> Tile -> Tile
moveTo point tile = tile & hexagon %~ (Hex.moveTo point)

moveBy :: Pict.Point -> Tile -> Tile
moveBy point tile = tile & hexagon %~ (Hex.moveBy point)

sideValue :: HexDirection -> Tile -> Int
sideValue dir tile = case dir of
  North -> a
  Northeast -> a
  Southeast -> b
  South -> b
  Southwest -> c
  Northwest -> c
  where (a,b,c) = view faces tile

changeSide :: HexDirection -> Int -> Tile -> Tile
changeSide dir val tile = tile & faces .~ case dir of
  North -> (val, b, c)
  Northeast -> (val, b, c)
  Southeast -> (a, val, c)
  South -> (a, val, c)
  Southwest -> (a, b, val)
  Northwest -> (a, b, val)
  where (a, b, c) = view faces tile

-- utility functions
facesRotations :: [Float]
facesRotations = [0, 120, 240]

contains :: Pict.Point -> Tile -> Bool
contains pos = Hex.contains pos . view hexagon
