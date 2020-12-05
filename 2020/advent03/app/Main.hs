module Main where

import AdventOfCode

main :: IO ()
main = do
  putStrLn "reading input.txt..."
  input <- readFile "input.txt"
  let m = parseMap $ lines input
      resultOne = partOne m
      resultTwo = partTwo m
  putStrLn $ "solution for part one is " ++ show resultOne
  putStrLn $ "solution for part two is " ++ show resultTwo

partOne :: Map -> Int
partOne m = traverseM m (down 1 . right 3)

partTwo :: Map -> Int
partTwo m = foldl (*) 1 $ map (traverseM m) [step1, step2, step3, step4, step5]
  where
    step1 = down 1 . right 1
    step2 = down 1 . right 3
    step3 = down 1 . right 5
    step4 = down 1 . right 7
    step5 = down 2 . right 1

traverseM :: Map -> (Position -> Position) -> Int
traverseM m step = length $ filter isTree $ goToBottom m (Position 0 0) step

goToBottom :: Map -> Position -> (Position -> Position) -> [Item]
goToBottom m pos step = h m pos step [] False
  where
    h m pos step res True = res
    h m pos step res False = h m (step pos) step (res ++ [positionItem m pos]) (atBottom m (step pos))
