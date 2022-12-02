module Lib
 where

import Data.List.Split

getLines :: String -> IO [String]
getLines f = lines <$> readFile f

groupByEmptyLine :: [String] -> [[String]]
groupByEmptyLine = splitWhen (== "")

asIntList :: [String] -> [Int]
asIntList = map read

day01 :: IO ()
day01 = do
  inp <- getLines "inputs/input01.txt"
  let m = maximum $ map (sum . asIntList) $ groupByEmptyLine inp
  putStrLn $ show m
