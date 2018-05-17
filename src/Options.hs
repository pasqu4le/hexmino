module Options where

import Options.Applicative
import Data.Semigroup ((<>))

newtype Options = Options {fps :: Int}

-- argument parsing
getOptions :: IO Options
getOptions = execParser $ info (opts <**> helper) (
    fullDesc
    <> header "Hexmino: put the hex-tiles in the grid as fast as possible"
    <> progDesc "A small game based on domino-like hexagonal tiles"
  )

opts :: Parser Options
opts = Options
  <$> option auto
    ( long "fps"
    <> short 'f'
    <> metavar "INT"
    <> help "Frames per second"
    <> showDefault
    <> value 60)
