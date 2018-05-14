module Widgets where

import qualified Hex
import qualified Table
import qualified Graphics.Gloss.Data.Picture as Pict
import qualified Graphics.Gloss.Data.Color as Color
import qualified Graphics.Gloss.Data.Point as Point
import Text.Printf (printf)
import Data.List (intercalate)

data Name = Banner | Level | RightArrow | LeftArrow | NewGame | Time | Completed |
  TimeRes | Player | Delete | Submit | LeaderLabel | LeaderSep | LeaderEntry Int |
  InfoLine Int | Info | CloseInfo | CloseGame

-- sizes and positions
shape :: Name -> (Float, Float, Float, Float)
shape name = case name of
  Banner -> (Table.gridX, 100, 350, 80)
  Level -> (Table.gridX, -100, 250, 30)
  RightArrow -> (Table.gridX + 150, -100, 40, 40)
  LeftArrow -> (Table.gridX - 150, -100, 40, 40)
  NewGame -> (Table.gridX, -150, 150, 40)
  Time -> (-270, 220, 160, 30)
  Completed -> (Table.gridX, 120, 200, 30)
  TimeRes -> (Table.gridX, 80, 300, 50)
  Player -> (Table.gridX -30, -100, 90, 30)
  Delete -> (Table.gridX +50, -100, 40, 30)
  Submit -> (Table.gridX, -150, 150, 40)
  LeaderLabel -> (Table.listX, 200, 200, 40)
  LeaderSep -> (Table.listX, 180, 200, 5)
  LeaderEntry n -> (Table.listX, 160 - 40 * fromIntegral n, 230, 45)
  InfoLine n -> (Table.gridX, 40 - 35 * fromIntegral n, 470, 40)
  Info -> (-320, -220, 30, 50)
  CloseInfo -> (Table.gridX, -150, 80, 40)
  CloseGame -> (-320, -220, 30, 50)

-- rendering
renderButton :: Name -> String -> Pict.Picture
renderButton name txt = Pict.translate x y $ Pict.pictures [
    Pict.color buttonColor $ Hex.rectangleBlunt w h,
    Pict.color Color.white $ Hex.hexagonText w h txt
  ]
  where (x, y, w, h) = shape name

renderLabel :: Name -> String -> Pict.Picture
renderLabel name txt = Pict.translate x y $ Pict.pictures [
    Pict.color labelColor $ Pict.rectangleSolid w h,
    Pict.color Color.white $ Hex.hexagonText w h txt
  ]
  where (x, y, w, h) = shape name

renderText :: Name -> String -> Pict.Picture
renderText name = Pict.translate x y . Pict.color Color.white . Hex.hexagonText w h
  where (x, y, w, h) = shape name

renderBanner :: Pict.Picture
renderBanner = renderText Banner "hexmino"

renderGameSelector :: (Show a, Eq a, Enum a, Bounded a) => a -> Pict.Picture
renderGameSelector sel = Pict.pictures [
    renderLabel Level $ show sel,
    renderButton NewGame "new game",
    if sel == minBound then Pict.Blank else renderButton LeftArrow "<",
    if sel == maxBound then Pict.Blank else renderButton RightArrow ">"
  ]

renderTime :: Float -> Pict.Picture
renderTime = renderLabel Time . secsToString

renderCompleted :: Float -> Pict.Picture
renderCompleted secs = Pict.pictures [
    renderText Completed "completed in",
    renderText TimeRes $ secsToString secs
  ]

renderNameSelector :: String -> Pict.Picture
renderNameSelector name = Pict.pictures [
    renderLabel Player txt,
    renderButton Delete "del",
    renderButton Submit "submit"
  ]
  where
    l = length name
    txt = if l < 3 then name ++ ('_' : replicate (2-l) ' ') else take 3 name

renderInfo :: Pict.Picture
renderInfo = Pict.pictures [
    renderButton CloseInfo "back",
    Pict.pictures $ zipWith renderText (map InfoLine [0..]) infoText
  ]

infoText :: [String]
infoText = [
    "drag the tiles on the grid",
    "rotate a tile using space",
    "neighbouring sides must match",
    "",
    "can you solve the expert puzzle?"
  ]

renderInfoButton :: Pict.Picture
renderInfoButton = renderButton Info "?"

renderCloseGame :: Pict.Picture
renderCloseGame = renderButton CloseGame "<"

renderTopTen :: [String] -> Pict.Picture
renderTopTen topTen = Pict.pictures [
    renderText LeaderLabel "leaderboard",
    renderLabel LeaderSep "",
    Pict.pictures $ zipWith renderText (map LeaderEntry [0..]) topTen
  ]

buttonColor :: Color.Color
buttonColor = Color.light Color.blue

labelColor :: Color.Color
labelColor = Color.dark Color.azure

-- click checking
findClicked :: Pict.Point -> [Name] -> Maybe Name
findClicked pos names = case filter (isClicked pos) names of
  [] -> Nothing
  (x:xs) -> Just x

isClicked :: Pict.Point -> Name -> Bool
isClicked pos name = Point.pointInBox pos (x - w/2, y - h/2) (x + w/2, y + h/2)
  where (x, y, w, h) = shape name

-- utility functions
secsToString :: Float -> String
secsToString secs = intercalate "/" vals
  where
    s = floor secs :: Int
    vals = map (printf "%02d" . (`mod` 60)) [s `div` 3600, s `div` 60, s]