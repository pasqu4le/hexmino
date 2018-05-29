module Score where

import Control.Lens hiding (Level)
import Text.Printf (printf)
import Data.List (intercalate, sort)
import Data.Char (isLetter)
import qualified System.Directory as Dir
import System.FilePath ((</>))

data Score = Score {
    _player :: String,
    _level :: Level,
    _time :: Float
  } deriving (Show, Read, Eq)
data Level = Beginner | Average | Expert deriving (Show, Read, Eq, Enum, Bounded)
type Leaderboard = [Score]

instance Ord Score where
  compare sc1 sc2
    | view level sc1 /= view level sc2 = compare (view level sc1) (view level sc2)
    | otherwise = compare (view time sc1) (view time sc2)

instance Ord Level where
  compare lv1 lv2 = compare (fromEnum lv2) (fromEnum lv1)

-- lenses
player :: Lens' Score String
player = lens _player (\score pl -> score {_player = pl})

level :: Lens' Score Level
level = lens _level (\score lvl -> score {_level = lvl})

time :: Lens' Score Float
time = lens _time (\score tm -> score {_time = tm})

-- creation
readPlayer :: IO Score
readPlayer = do
  dataDir <- dataDirectory
  let filePath = dataDir </> "player"
  exists <- Dir.doesFileExist filePath
  if exists then
    read <$> readFile filePath
  else
    return $ Score {_player = "PAS", _level = Beginner, _time = 0}

readTopTen :: IO Leaderboard
readTopTen = take 10 <$> readLeaderboard

-- manipulation
clearTime :: Score -> Score
clearTime = set time 0

delFromName :: Score -> Score
delFromName = over player init

addToName :: Char -> Score -> Score
addToName c score
  | isLetter c = score & player %~ (++ [c]) . take 2
  | otherwise = score

submit :: Score -> IO Leaderboard
submit score = do
  dataDir <- dataDirectory
  let playerPath = dataDir </> "player"
      leaderPath = dataDir </> "leaderboard"
      newLeaderPath = dataDir </> "leaderboard.new"
  writeFile playerPath . show $ clearTime score
  leaders <- readLeaderboard
  let newLeaders = sort (score:leaders)
  writeFile newLeaderPath . unlines $ map show newLeaders
  Dir.renameFile newLeaderPath leaderPath
  return $ take 10 newLeaders

-- persistency
dataDirectory :: IO FilePath
dataDirectory = do
  dataDir <- Dir.getXdgDirectory Dir.XdgData "hexmino"
  Dir.createDirectoryIfMissing True dataDir
  return dataDir

readLeaderboard :: IO Leaderboard
readLeaderboard = do
  dataDir <- dataDirectory
  let filePath = dataDir </> "leaderboard"
  exists <- Dir.doesFileExist filePath
  if exists then do
    info <- readFile filePath
    let (name:lvl:_) = words info
    map read . lines <$> readFile filePath
  else
    return [Score {_player = "PAS", _level = Expert, _time = 2520}]

-- stepping
step :: Float -> Score -> Score
step secs = time +~ secs

-- utility
display :: Score -> String
display score = unwords [
    view player score, 
    secsToString $ view time score, 
    levelShort $ view level score
  ]

levelNum :: Score -> Int
levelNum = (1+) . fromEnum . view level

toNextLevel :: Score -> Score
toNextLevel score
  | isMaxLevel $ view level score = score
  | otherwise = score & level %~ succ

toPreviousLevel :: Score -> Score
toPreviousLevel score
  | isMinLevel $ view level score = score
  | otherwise = score & level %~ pred

levelShort :: Level -> String
levelShort lvl = case lvl of
  Beginner -> "B"
  Average -> "A"
  Expert -> "X"

showTime :: Score -> String
showTime = secsToString . view time

secsToString :: Float -> String
secsToString secs = intercalate "/" vals
  where
    s = floor secs :: Int
    vals = map (printf "%02d" . (`mod` 60)) [s `div` 3600, s `div` 60, s]

isMinLevel :: Level -> Bool
isMinLevel = (== minBound)

isMaxLevel :: Level -> Bool
isMaxLevel = (== maxBound)
