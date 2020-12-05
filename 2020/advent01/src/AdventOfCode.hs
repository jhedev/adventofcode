module AdventOfCode
    ( find2Numbers
    , find3Numbers
    ) where

find2Numbers :: [Int] -> Int -> (Int, Int)
find2Numbers is target = head [(a,b) | a <- is, b <- is, a+b == target]

find3Numbers :: [Int] -> Int -> (Int, Int, Int)
find3Numbers is target = head [(a,b,c) | a <- is, b <- is, c <-is, a+b+c == target]
