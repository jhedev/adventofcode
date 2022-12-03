module Lib
 where

import Data.List.Split

getLines :: String -> IO [String]
getLines f = lines <$> readFile f


groupByEmptyLine :: [String] -> [[String]]
groupByEmptyLine = splitWhen (== "")

asIntList :: [String] -> [Int]
asIntList = map read

-- Day 01

day01 :: IO Int
day01 = do
  inp <- getLines "inputs/input01.txt"
  return $ maximum $ map (sum . asIntList) $ groupByEmptyLine inp

-- Day 02

day02 :: IO Int
day02 = do
  inp <- getLines "inputs/input02.txt"
  let scores = map (evalRound . parseLine) inp
  return $ sum scores

data Choice = Rock | Paper | Scissors deriving Eq
data Round = Round { opponent :: Choice, me :: Choice }

parseLine :: String -> Round
parseLine l = Round { opponent = op, me = me}
  where
    [o,m] = words l
    op = case o of
      "A" -> Rock
      "B" -> Paper
      "C" -> Scissors
    me = case m of
      "X" -> Rock
      "Y" -> Paper
      "Z" -> Scissors

evalRound :: Round -> Int
evalRound r = scoreByChoice (me r) + win r

win :: Round -> Int
win Round{opponent=Rock,me=Paper} = 6
win Round{opponent=Paper,me=Scissors} = 6
win Round{opponent=Scissors,me=Rock} = 6
win Round{opponent=o,me=m} = if o == m then 3 else 0

scoreByChoice :: Choice -> Int
scoreByChoice Rock     = 1
scoreByChoice Paper    = 2
scoreByChoice Scissors = 3
