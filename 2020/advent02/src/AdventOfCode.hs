module AdventOfCode
    ( Policy(..)
    , Password
    , Entry(..)
    , parseLineMinMax
    , parseLinePosition
    , validate
    ) where

import Data.List.Split

data Policy = MinMaxPolicy Int Int Char
            | PositionPolicy Int Int Char

type Password = String

data Entry = Entry {
  policy :: Policy,
  password :: Password
  }

parseLine :: (Int -> Int -> Char -> Policy) -> String -> Entry
parseLine f line = Entry p pw
  where
    [range, char, pw] = words line
    p = f (read min) (read max) c
    [min, max] = splitOn "-" range
    c = head char

parseLineMinMax :: String -> Entry
parseLineMinMax = parseLine MinMaxPolicy

parseLinePosition :: String -> Entry
parseLinePosition = parseLine PositionPolicy

validate :: Policy -> Password -> Bool
validate (MinMaxPolicy min max char) pw = min <= count && max >= count
  where
    counter c v = if v == c then 1 else 0
    count = sum $ map (counter char) pw
validate (PositionPolicy one two char) pw = l == 1
  where
    l = length $ filter (== char) $ map snd $ filter (\(a,b) -> a == one || a == two) $ zip [1..] pw
