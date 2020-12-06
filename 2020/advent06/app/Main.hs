module Main where

import AdventOfCode

main :: IO ()
main = do
  input <- readFile "input.txt"
  let groups = parseFile input
      resultOne = sum . map (length . anyoneYes) $ groups
      resultTwo = sum . map (length . everyoneYes) $ groups
  putStrLn $ "solution for part one is " ++ show resultOne
  putStrLn $ "solution for part two is " ++ show resultTwo
