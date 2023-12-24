module Day01 (main) where

import AoC (aocmain,Settings)

main :: IO ()
main = do
  inp <- aocmain Settings {...
                     }
  let calories = reverse $ sort $ map (sum . asIntList) $ groupByEmptyLine inp
      part01 = head calories
      part02 = sum $ take 3 calories
  return (part01,part02)
