module Hex where

import qualified Graphics.Gloss.Data.Picture as Pict

data Hexagon = Hexagon {center :: Pict.Point, radius :: Float} deriving (Show)

-- rendering functions
render :: Hexagon -> Pict.Picture
render hex = Pict.translate x y $ renderCentered hex
  where (x, y) = center hex

renderCentered :: Hexagon -> Pict.Picture
renderCentered = hexagonSolid . radius

hexagonSolidPointy :: Float -> Pict.Picture
hexagonSolidPointy = Pict.rotate 30 . hexagonSolid

hexagonSolid :: Float -> Pict.Picture
hexagonSolid = Pict.polygon . hexagonPath

hexagonPath :: Float -> Pict.Path
hexagonPath r = [(-r, 0), (-rx, ry), (rx, ry), (r, 0), (rx, -ry), (-rx, -ry)]
  where
    rx = r / 2
    ry = heightFromRadius r / 2

hexagonRect :: Float -> Float -> Pict.Picture
hexagonRect w h = Pict.polygon [(-sw,hh),(-hw,sh),(-hw,-sh),(-sw,-hh),(sw,-hh),(hw,-sh),(hw,sh),(sw,hh)]
  where
    c = w / 10
    hw = w / 2
    hh = h / 2
    sw = hw - c
    sh = hh - c

-- manupulation functions
moveTo :: Pict.Point -> Hexagon -> Hexagon
moveTo point hex = hex {center = point}

moveBy :: Pict.Point -> Hexagon -> Hexagon
moveBy (x, y) hex = hex {center = (cx+x, cy+y)}
  where (cx, cy) = center hex

-- utility functions
hexagonHeight :: Hexagon -> Float
hexagonHeight = heightFromRadius . radius

heightFromRadius :: Float -> Float
heightFromRadius = (* sqrt 3)

contains :: Pict.Point -> Hexagon -> Bool -- approximation, but good enough
contains pos hex
  | distanceFromCenter pos hex < hexagonHeight hex / 2 = True
  | otherwise = False

distanceFromCenter :: Pict.Point -> Hexagon -> Float
distanceFromCenter pnt Hexagon {center = cnt} = pointsDistance pnt cnt

pointsDistance :: Pict.Point -> Pict.Point -> Float
pointsDistance (x1,y1) (x2,y2) = sqrt $ (x1-x2) ** 2 + (y1-y2) ** 2
