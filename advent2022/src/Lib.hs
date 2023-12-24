module Lib
 where

import Data.Char (ord)
import Data.List (sort)
import Data.List.Split (splitWhen, chunksOf)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

getLines :: String -> IO [String]
getLines f = lines <$> readFile f

groupByEmptyLine :: [String] -> [[String]]
groupByEmptyLine = splitWhen (== "")

asIntList :: [String] -> [Int]
asIntList = map read

mapPair :: (a -> b, c -> d) -> (a,c) -> (b,d)
mapPair (f1,f2) (t1,t2) = (f1 t1, f2 t2)

-- Day 01

day01 :: IO (Int,Int)
day01 = do
  inp <- getLines "inputs/input01.txt"
  let calories = reverse $ sort $ map (sum . asIntList) $ groupByEmptyLine inp
      part01 = head calories
      part02 = sum $ take 3 calories
  return (part01,part02)

-- Day 02

day02 :: IO (Int,Int)
day02 = do
  inp <- getLines "inputs/input02.txt"
  let part01 = sum $ map (evalRound . parseLine) inp
      part02 = sum $ map (uncurry (+) . mapPair (scoreByChoice, scoreByResult) . mkChoice . parseLine2) inp
  return (part01, part02)

data Choice = Rock | Paper | Scissors deriving (Show, Eq)
data Round = Round { opponent :: Choice, me :: Choice } deriving (Show, Eq)
data Result = Win | Draw | Loss deriving (Show, Eq)

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
evalRound r = scoreByChoice (me r) + scoreByResult (eval r)

eval :: Round -> Result
eval Round{opponent=Rock,me=Paper} = Win
eval Round{opponent=Paper,me=Scissors} = Win
eval Round{opponent=Scissors,me=Rock} = Win
eval Round{opponent=o,me=m} = if o == m then Draw else Loss

scoreByChoice :: Choice -> Int
scoreByChoice Rock     = 1
scoreByChoice Paper    = 2
scoreByChoice Scissors = 3

scoreByResult :: Result -> Int
scoreByResult Win  = 6
scoreByResult Draw = 3
scoreByResult Loss = 0

parseLine2 :: String -> (Choice, Result)
parseLine2 [op,' ',r] = (opponent,result)
  where
    opponent = case op of
      'A' -> Rock
      'B' -> Paper
      'C' -> Scissors
    result = case r of
      'X' -> Loss
      'Y' -> Draw
      'Z' -> Win

mkChoice :: (Choice,Result) -> (Choice,Result)
mkChoice (Rock,Win) = (Paper,Win)
mkChoice (Rock,Loss) = (Scissors,Loss)
mkChoice (Paper,Win) = (Scissors,Win)
mkChoice (Paper,Loss) = (Rock,Loss)
mkChoice (Scissors,Win) = (Rock,Win)
mkChoice (Scissors,Loss) = (Paper,Loss)
mkChoice c = c


-- Day 03

day03 :: IO (Int, Int)
day03 = do
  inp <- getLines "inputs/input03.txt"
  let items = map parseItems inp
      part01 = sum $ map (priority . commonItem . mkRucksack) items
      part02 = sum $ map (priority . commonInGroup) $ chunksOf 3 items
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

-- Day 04

day04 :: IO (Int, Int)
day04 = do
  content <- readFile "inputs/input04.txt"
  let Right pairs = parseRanges content
      part01 = sum $ map (fromEnum . inclusion) pairs
      part02 = sum $ map (fromEnum . overlap) pairs
  return (part01,part02)


data Range = Range Int Int deriving (Show,Eq)
data RangePair = RangePair Range Range deriving (Show,Eq)

inclusion :: RangePair -> Bool
inclusion (RangePair (Range from1 to1) (Range from2 to2)) = from1 <= from2 && to1 >= to2 || from1 >= from2 && to1 <= to2

overlap :: RangePair -> Bool
overlap (RangePair (Range from1 to1) (Range from2 to2))=
  not(to1 < from2 || from1 > to2)

parseRanges :: String -> Either ParseError [RangePair]
parseRanges = parse rangeParser ""

rangeParser :: GenParser Char st [RangePair]
rangeParser = do
  ls <- many rangePairLine
  eof
  return ls

rangePairLine :: GenParser Char st RangePair
rangePairLine = do
  first <- range
  _ <- char ','
  second <- range
  _ <- char '\n'
  return $ RangePair first second

range :: GenParser Char st Range
range = do
  from <- read <$> many1 digit
  _ <- char '-'
  to <- read <$> many1 digit
  return $ Range from to



-- day05

day05 :: IO (String, Int)
day05 = do
  content <- readFile "inputs/input05.txt"
  let part01 = ""
      part02 = 0
  return (part01, part02)


-- First element is highest element, last element is lowest
data Stack = Stack String deriving (Show, Eq)
data Movement = Movement { number :: Int, from :: Int, to :: Int } deriving (Show, Eq)

data StacksMovements = StacksMovements { stacks :: [Stack], movements :: [Movement]} deriving (Show, Eq)

applyMovement :: (String -> String) -> [Stack] -> Movement -> [Stack]
applyMovement func st mov = Map.elems $
                         Map.adjust (elems++) t $
                         Map.adjust (drop n) f m
  where
    n = number mov
    f = from mov
    t = to mov
    m = Map.fromList $ zip [1..] st
    elems = func $ take n $ Map.(!) m f


topCrates :: [Stack] -> [Crate]
topCrates = map (\(Stack s) -> head s)

parseDay05 :: String -> Either ParseError [StacksMovements]
parseDay05 = undefined
