module Score where

import Text.Printf (printf)
import Data.List (intercalate, sort)
import Data.Char (isLetter)
import qualified System.Directory as Dir
import System.FilePath ((</>))

data Score = Score {player :: String, level :: Level, time :: Float} deriving (Show, Read, Eq)
data Level = Beginner | Average | Expert deriving (Show, Read, Eq, Enum, Bounded)
type Leaderboard = [Score]

instance Ord Score where
  compare sc1 sc2
    | level sc1 /= level sc2 = compare (level sc1) (level sc2)
    | otherwise = compare (time sc1) (time sc2)

instance Ord Level where
  compare lv1 lv2 = compare (fromEnum lv2) (fromEnum lv1)

-- creation
readPlayer :: IO Score
readPlayer = do
  dataDir <- dataDirectory
  let filePath = dataDir </> "player"
  exists <- Dir.doesFileExist filePath
  if exists then do
    read <$> readFile filePath
  else
    return $ Score {player = "PAS", level = Beginner, time = 0}

readTopTen :: IO Leaderboard
readTopTen = take 10 <$> readLeaderboard

-- manipulation
clearTime :: Score -> Score
clearTime score = score {time = 0}

delFromName :: Score -> Score
delFromName score = score {player = init $ player score}

addToName :: Char -> Score -> Score
addToName c score
  | isLetter c = score {player = take 2 (player score) ++ [c]} 
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
    return [Score {player = "PAS", level = Expert, time = 2520}]

-- stepping
step :: Float -> Score -> Score
step secs score = score {time = secs + time score}

-- utility
display :: Score -> String
display score = unwords [
    player score, 
    secsToString $ time score, 
    levelShort $ level score
  ]

levelNum :: Score -> Int
levelNum = (1+) . fromEnum . level

toNextLevel :: Score -> Score
toNextLevel score
  | level score == maxBound = score
  | otherwise = score {level = succ $ level score}

toPreviousLevel :: Score -> Score
toPreviousLevel score
  | level score == minBound = score
  | otherwise = score {level = pred $ level score}

levelShort :: Level -> String
levelShort lvl = case lvl of
  Beginner -> "B"
  Average -> "A"
  Expert -> "X"

showTime :: Score -> String
showTime = secsToString . time

secsToString :: Float -> String
secsToString secs = intercalate "/" vals
  where
    s = floor secs :: Int
    vals = map (printf "%02d" . (`mod` 60)) [s `div` 3600, s `div` 60, s]

isMinLevel :: Level -> Bool
isMinLevel lvl = lvl == minBound

isMaxLevel :: Level -> Bool
isMaxLevel lvl = lvl == maxBound
