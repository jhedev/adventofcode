module Main where

import AdventOfCode

main :: IO ()
main = do
  putStrLn "reading input.txt..."
  input <- readFile "input.txt"
  let ps = parseFile input
      resultOne = length $ filter id $ map checkRequiredFields ps
      resultTwo = length $ filter id $ map checkFieldsValid ps
  putStrLn $ "solution of part one is " ++ show resultOne
  putStrLn $ "solution of part two is " ++ show resultTwo
