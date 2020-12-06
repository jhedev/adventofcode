module AdventOfCode
    ( Group
    , questions
    , numPersons
    , anyoneYes
    , everyoneYes
    , parseLine
    , parseGroup
    , parseFile
    ) where

import qualified Data.HashMap.Strict as Map
import Data.List.Split

data Group = Group Int (Map.HashMap Char Int) deriving Show

questions :: Group -> [Char]
questions (Group _ m) = Map.keys m

numPersons :: Group -> Int
numPersons (Group p _) = p

anyoneYes :: Group -> [Char]
anyoneYes (Group _ m) = Map.keys m

everyoneYes :: Group -> [Char]
everyoneYes (Group p m) = Map.foldlWithKey' f [] m
  where
    f acc q i | p == i = q:acc
              | otherwise = acc

parseLine :: Map.HashMap Char Int -> String -> Map.HashMap Char Int
parseLine = foldl f
  where
    f :: Map.HashMap Char Int -> Char -> Map.HashMap Char Int
    f m c = Map.insertWith (\_ o -> o+1) c 1 m

parseGroup :: [String] -> Group
parseGroup persons = Group (length persons) $ foldl parseLine Map.empty persons

parseFile :: String -> [Group]
parseFile = map parseGroup . splitWhen (== "") . lines
