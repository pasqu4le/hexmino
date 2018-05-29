module Selection where

import qualified Tile

import Control.Lens
import qualified Graphics.Gloss.Data.Picture as Pict

-- a selection is a tile that is currently controlled by the user
data Selection = Selection {
    _tile :: Tile.Tile,
    _rotation :: Maybe Float
  }

-- lenses
tile :: Lens' Selection Tile.Tile
tile = lens _tile (\selection tl -> selection {_tile = tl})

rotation :: Lens' Selection (Maybe Float)
rotation = lens _rotation (\selection rt -> selection {_rotation = rt})

-- creation
make :: Tile.Tile -> Selection
make tl = Selection {_tile = tl, _rotation = Nothing}

-- rendering
render :: Selection -> Pict.Picture
render sel = case view rotation sel of
  Just rot -> Tile.renderRotated rot $ view tile sel
  _ -> Tile.render $ view tile sel

-- manupulation functions
rotate :: Selection -> Selection
rotate = set rotation (Just 0) -- only starts the rotation, needs to be stepped

moveTo :: Pict.Point -> Selection -> Selection
moveTo pos = over tile (Tile.moveTo pos)

-- stepping
step :: Float -> Selection -> Selection
step secs sel = case view rotation sel of
  Just rot -> let newRot = rot + secs * 1200 in if newRot >= 120 then
    sel & tile %~ Tile.rotate & rotation .~ Nothing
    else sel & rotation .~ Just newRot
  _ -> sel
