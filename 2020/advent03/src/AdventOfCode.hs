module AdventOfCode
    ( Item(..)
    , parseItem
    , isTree
    , Line(..)
    , parseLine
    , Map
    , parseMap
    , Position(..)
    , positionItem
    , right
    , down
    , atBottom
    ) where


data Item = Tree | Square
  deriving (Show)

parseItem :: Char -> Item
parseItem '#' = Tree
parseItem '.' = Square
parseItem _ = undefined

isTree :: Item -> Bool
isTree Tree = True
isTree _    = False

data Line = Line [Item]
  deriving (Show)

parseLine :: String -> Line
parseLine = Line . map parseItem

positionInLine :: Int -> Line -> Item
positionInLine y (Line is) = is !! (y `mod` (length is))

data Map  = Map [Line]
 deriving (Show)

parseMap :: [String] -> Map
parseMap = Map . map parseLine

data Position = Position Int Int
  deriving (Show)

positionItem :: Map -> Position -> Item
positionItem (Map ls) (Position x y) = item
  where
    l = length ls
    line = ls !! ( y `mod` l)
    item = positionInLine x line

atBottom :: Map -> Position -> Bool
atBottom (Map ls) (Position _ y) = y >= length ls

right :: Int -> Position -> Position
right i (Position x y) = Position (x+i) y

down :: Int -> Position -> Position
down i (Position x y) = Position x (y+i)
