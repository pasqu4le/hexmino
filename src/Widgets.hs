module Widgets where

import qualified Hex
import qualified Table
import qualified Graphics.Gloss.Data.Picture as Pict
import qualified Graphics.Gloss.Data.Color as Color

-- rendering
renderBanner :: Pict.Picture
renderBanner = Pict.translate Table.gridX 100 $ Pict.pictures [
    Pict.color Color.white $ Hex.rectangleBlunt w h,
    Pict.color Color.black $ Hex.hexagonText w h "hexmino"
  ]
  where (w,h) = (350, 80)

renderGameSelector :: (Show a, Eq a, Enum a, Bounded a) => a -> Pict.Picture
renderGameSelector sel = Pict.pictures [
    Pict.translate Table.gridX (-100) . Pict.color labelColor $ Pict.rectangleSolid lw lh,
    Pict.translate Table.gridX (-100) . Pict.color Color.white $ Hex.hexagonText lw lh txt,
    Pict.translate Table.gridX (-150) . Pict.color buttonColor $ Hex.rectangleBlunt bw bh,
    Pict.translate Table.gridX (-150) . Pict.color Color.white $ Hex.hexagonText bw bh "new game",
    Pict.translate (Table.gridX - 150) (-100) buttonLeft,
    Pict.translate (Table.gridX + 150) (-100) buttonRight
  ]
  where
    (lw, lh) = (250, 30)
    (bw, bh) = (150, 40)
    txt = show sel
    buttonLeft = if sel == minBound then Pict.Blank else arrowLeft
    buttonRight = if sel == maxBound then Pict.Blank else arrowRight

arrowLeft :: Pict.Picture
arrowLeft = Pict.rotate 180 arrowRight

arrowRight :: Pict.Picture
arrowRight = Pict.pictures [
    Pict.color buttonColor $ Hex.rectangleBlunt 40 40,
    Pict.color Color.white $ Hex.hexagonText 40 40 ">"
  ]

renderTime :: String -> Pict.Picture
renderTime = Pict.translate (-270) 220 . Pict.color Color.black . Hex.hexagonText 160 30

renderTimeRes :: String -> Pict.Picture
renderTimeRes time = Pict.pictures [
    Pict.translate Table.gridX 120 . Pict.color Color.white $ Hex.hexagonText lw lh "completed in",
    Pict.translate Table.gridX 80 . Pict.color labelColor $ Pict.rectangleSolid tw th,
    Pict.translate Table.gridX 80 . Pict.color Color.white $ Hex.hexagonText tw th time
  ]
  where 
    (lw, lh) = (200, 30)
    (tw, th) = (300, 50)

renderNameSelector :: String -> Pict.Picture
renderNameSelector name = Pict.pictures [
    Pict.translate (Table.gridX -30) (-100) . Pict.color labelColor $ Pict.rectangleSolid lw lh,
    Pict.translate (Table.gridX -30) (-100) . Pict.color Color.white $ Hex.hexagonText lw lh txt,
    Pict.translate (Table.gridX +50) (-100) . Pict.color buttonColor $ Hex.rectangleBlunt bw bh,
    Pict.translate (Table.gridX +50) (-100) . Pict.color Color.white $ Hex.hexagonText bw bh "del",
    Pict.translate Table.gridX (-150) . Pict.color buttonColor $ Hex.rectangleBlunt sw sh,
    Pict.translate Table.gridX (-150) . Pict.color Color.white $ Hex.hexagonText sw sh "submit"
  ]
  where
    (lw, lh) = (90, 30)
    (bw, bh) = (40, 30)
    (sw, sh) = (150, 40)
    l = length name
    txt = if l < 3 then name ++ ('_' : replicate (2-l) ' ') else take 3 name

renderInfo :: Pict.Picture
renderInfo = Pict.Blank -- TODO

renderTopTen :: [String] -> Pict.Picture
renderTopTen topTen = Pict.pictures $ [
    Pict.translate Table.listX 200 . Pict.color Color.white $ Hex.hexagonText bw bh "leaderboard",
    Pict.translate Table.listX 180 . Pict.color labelColor $ Pict.rectangleSolid bw 5
  ] ++ leads
  where
    (bw, bh) = (200, 40)
    (lw, lh) = (230, 45)
    leads = zipWith (Pict.translate Table.listX) [160,120..] $ map (Pict.color Color.white . Hex.hexagonText lw lh) topTen

buttonColor :: Color.Color
buttonColor = Color.light Color.blue

labelColor :: Color.Color
labelColor = Color.dark Color.azure
