module Hex where

import Data.Char (toUpper)
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

rectangleBlunt :: Float -> Float -> Pict.Picture
rectangleBlunt w h = Pict.polygon $ rectangleBluntPath w h

rectangleBluntPath :: Float -> Float -> Pict.Path
rectangleBluntPath w h = [(-sw,hh),(-hw,sh),(-hw,-sh),(-sw,-hh),(sw,-hh),(hw,-sh),(hw,sh),(sw,hh)]
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

-- string (contained in a rectangle) and letter with hexagonal shape

hexagonText :: Float -> Float -> String -> Pict.Picture
hexagonText w h txt = Pict.scale fa 1 . Pict.translate offset 0 $ Pict.pictures letters
  where
    r = h / 3
    spacing = r * 2.5
    size = length txt
    letters = zipWith (`Pict.translate` 0) [0,spacing..] $ map (hexagonChar r) txt
    offset = (-spacing) * fromIntegral (size - 1) / 2
    fa = w / (fromIntegral size * r * 3)


hexagonChar :: Float -> Char -> Pict.Picture
hexagonChar r letter = thickLine (r/4) $ case toUpper letter of
  'A' -> [sw, w, nw, ne, e, w, e, se]
  'B' -> [e, se, sw, nw, ne, e, c]
  'C' -> [ne, nw, w, sw, se] 
  'D' -> [sw, nw, ne, e, se, sw]
  'E' -> [ne, nw, w, c, w, sw, se]
  'F' -> [ne, nw, w, c, w, sw]
  'G' -> [ne, nw, w, sw, se, e, c]
  'H' -> [nw, w, sw, w, e, ne, e, se]
  'I' -> [nw, ne, n, s, se, sw]
  'J' -> [ne, e, se, sw, w]
  'K' -> [nw, w, sw, w, ne, w, se]
  'L' -> [nw, w, sw, se]
  'M' -> [sw, w, nw, c, ne, e, se]
  'N' -> [sw, w, nw, se, e, ne]
  'O' -> [w, nw, ne, e, se, sw, w]
  'P' -> [sw, w, nw, ne, e, w]
  'Q' -> [se, sw, w, nw, ne, e, se, c]
  'R' -> [sw, w, nw, ne, e, w, c, se]
  'S' -> [ne, nw, w, e, se, sw]
  'T' -> [s, n, nw, w, nw, ne, e]
  'U' -> [nw, w, sw, se, e, ne] 
  'V' -> [nw, w, s, e, ne]
  'W' -> [nw, w, sw, c, se, e, ne]
  'X' -> [nw, se, c, ne, sw]
  'Y' -> [nw, c, s, c, ne]
  'Z' -> [w, nw, ne, sw, se, e]
  '0' -> [nw, w, sw, se, e, ne, nw, se]
  '1' -> [nw, n, s]
  '2' -> [w, nw, ne, e, sw, se]
  '3' -> [nw, ne, c, e, se, sw]
  '4' -> [nw, w, e, ne, e, se]
  '5' -> [ne, nw, c, e, se, sw]
  '6' -> [ne, nw, w, e, se, sw, w]
  '7' -> [w, nw, ne, sw]
  '8' -> [c, nw, ne, c, e, se, sw, w, c]
  '9' -> [sw, se, e, ne, nw, w, e]
  '>' -> [nw, c, sw]
  '<' -> [ne, c, se]
  '_' -> [sw, se]
  '/' -> [sw, ne]
  '?' -> [w, nw, ne, e, c, s]
  ' ' -> []
  _ -> [ne, e, w, sw, ne]
  where
    [w, nw, ne, e, se, sw] = hexagonPath r
    c = (0,0)
    ry = heightFromRadius r / 2
    n = (0, ry)
    s = (0, -ry)

thickLine :: Float -> Pict.Path -> Pict.Picture
thickLine t = Pict.pictures . thickSegments t

thickSegments :: Float -> Pict.Path -> [Pict.Picture]
thickSegments t (a:b:xs) = Pict.polygon (thickSegmentPath t a b) : thickSegments t (b:xs)
thickSegments _ _ = []

thickSegmentPath :: Float -> Pict.Point -> Pict.Point -> Pict.Path
thickSegmentPath t (ax, ay) (bx, by)
  | abs dx >= abs dy && dx >= 0 = [(ax, ay+r),(ax-r, ay),(ax, ay-r),(bx, by-r),(bx+r, by),(bx, by+r)]
  | abs dx >= abs dy = [(ax, ay+r),(ax+r, ay),(ax, ay-r),(bx, by-r),(bx-r, by),(bx, by+r)]
  | dy >= 0 = [(ax-r, ay),(ax, ay-r),(ax+r, ay),(bx+r, by),(bx, by+r),(bx-r, by)]
  | otherwise = [(ax-r, ay),(ax, ay+r),(ax+r, ay),(bx+r, by),(bx, by-r),(bx-r, by)]
  where
    (dx, dy) = (bx - ax, by - ay)
    r = t / 2