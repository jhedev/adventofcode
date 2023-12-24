module AoC (aocmain, Settings(..)) where

import Text.ParserCombinators.Parsec

data Settings t r = Settings {
  inputNum  :: String,
  parser :: GenParser Char () t,
  part01 :: t -> r,
  part02 :: t -> r
}

aocmain :: Show r => Settings t r -> IO ()
aocmain set = do
  let p = parse (parser set) ""
  content <- readFile $ "inputs/input" <> (inputNum set) <> ".txt"
  let Right inp = p content
      p1 = part01 set $ inp
      p2 = part02 set $ inp
  putStrLn $ "part01=" <> (show p1)
  putStrLn $ "part02=" <> (show p2)
