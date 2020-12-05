module Main where

import AdventOfCode

main :: IO ()
main = do
  putStrLn "reading input file ..."
  input <- readFile "input.txt"
  let ls = lines input
      countOne = calc parseLineMinMax ls
      countTwo = calc parseLinePosition ls
  putStrLn $ "solution for part one is "  ++ show countOne
  putStrLn $ "solution for part two is "  ++ show countTwo

calc :: (String -> Entry) -> [String] -> Int
calc p entries = length $ filter id validation
  where
      validateEntry = \e -> validate (policy e) (password e)
      validation = map (validateEntry . p) entries
