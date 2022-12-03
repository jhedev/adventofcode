module Lib
 where

import Data.Char (ord)
import Data.List.Split (splitWhen, splitEvery)

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

-- Day 03

day03 :: IO (Int, Int)
day03 = do
  inp <- getLines "inputs/input03.txt"
  let items = map parseItems inp
      part01 = sum $ map (priority . commonItem . mkRucksack) items
      part02 = sum $ map (priority . commonInGroup) $ splitEvery 3 items
  return (part01,part02)

data Item = Item Char deriving (Eq, Show)
data Rucksack = Rucksack [Item] [Item] deriving (Eq, Show)

parseItems :: String -> [Item]
parseItems = map Item

mkRucksack :: [Item] -> Rucksack
mkRucksack is = Rucksack first second
  where
    (first,second) = splitAt (length is `div` 2) is

commonItem :: Rucksack -> Item
commonItem (Rucksack first second) = item
  where
    -- ignore if the item type is in one compartment multiple times
    item:_ = [i | i <- first, j <- second, i == j]

priority :: Item -> Int
priority (Item c) = if c `elem` ['a'..'z'] then ord c - 96 else ord c - 38

commonInGroup :: [[Item]] -> Item
commonInGroup [i1,i2,i3] = item
  where
    item:_ = [i | i <- i1, j <- i2, k <- i3, i == j, j == k]
