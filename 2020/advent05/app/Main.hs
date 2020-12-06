module Main where

import qualified Data.Set as Set

import AdventOfCode

main :: IO ()
main = do
  input <- readFile "input.txt"
  let seats = parseFile input
      seatIDs = map seatID seats
      maxSeatID = maximum seatIDs
      seatSet = Set.fromList seatIDs
      allSeats = Set.fromList [0..1024]
      diff = Set.difference allSeats seatSet
      mySeat = findSeat diff
  putStrLn $ "solution for part one is " ++ show maxSeatID
  putStrLn $ "solution for part two is " ++ show mySeat

findSeat :: Set.Set Int -> [Int]
findSeat s = Set.foldl f [] s
  where
    f acc i = if  Set.lookupGT i s == Just (i+1) || Set.lookupLT i s == Just (i-1)
              then acc
              else i:acc
