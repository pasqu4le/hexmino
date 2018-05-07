module Main where

import qualified Game
import qualified Options as Opts

-- only an entry point
main :: IO ()
main = Game.run =<< Opts.getOptions
