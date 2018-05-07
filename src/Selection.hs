module Selection where

import qualified Tile
import qualified Graphics.Gloss.Data.Picture as Pict

-- a selection is a tile that is currently controlled by the user
data Selection = Selection {tile :: Tile.Tile, rotation :: Maybe Float}

-- creation
make :: Tile.Tile -> Selection
make t = Selection {tile = t, rotation = Nothing}

-- rendering
render :: Selection -> Pict.Picture
render sel = case rotation sel of
  Just rot -> Tile.renderRotated rot $ tile sel
  _ -> Tile.render $ tile sel

-- manupulation functions
rotate :: Selection -> Selection
rotate sel = sel {rotation = Just 0} -- only starts the rotation, needs to be stepped

moveTo :: Pict.Point -> Selection -> Selection
moveTo pos sel = sel {tile = Tile.moveTo pos $ tile sel}

-- stepping
step :: Float -> Selection -> Selection
step secs sel = case rotation sel of
  Just rot -> let newRot = rot + secs * 1200 in if newRot >= 120 then
    sel {tile = Tile.rotate $ tile sel, rotation = Nothing}
    else sel {rotation = Just newRot}
  _ -> sel
