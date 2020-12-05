module Main where

import AdventOfCode

main :: IO ()
main = do
  putStrLn "Reading input.txt..."
  input <- readFile "input.txt"
  let ls = lines input
      numbers = map read ls

  putStrLn "+++ Part One +++"
  let solution1 = partOne numbers
  putStrLn $ "Solution is: " ++ show solution1
  putStrLn "+++ Part Two +++"
  let solution2 = partTwo numbers
  putStrLn $ "Solution is: " ++ show solution2

partOne :: [Int] -> Int
partOne numbers = a*b
  where (a,b) = find2Numbers numbers 2020

partTwo :: [Int] -> Int
partTwo numbers = a*b*c
  where (a,b,c) = find3Numbers numbers 2020
