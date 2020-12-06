module AdventOfCode
    ( RowPartition(..)
    , ColumnPartition(..)
    , parseFile
    , parseLine
    , partitionToSeat
    , Seat(..)
    , seatID
    , BinarySpace(..)
    , binarySpaceSearch
    ) where

import Data.List.Split
import Data.Maybe

data RowPartition = Front | Back deriving Show
data ColumnPartition = CLeft | CRight deriving Show

parseRowPartition :: Char -> Maybe RowPartition
parseRowPartition 'F' = Just Front
parseRowPartition 'B' = Just Back
parseRowPartition _   = Nothing

parseColumnPartition :: Char -> Maybe ColumnPartition
parseColumnPartition 'L' = Just CLeft
parseColumnPartition 'R' = Just CRight
parseColumnPartition _ = Nothing

parseFile :: String -> [Seat]
parseFile = map (mkSeat . fromJust) . filter isJust . map parseLine . lines

parseLine :: String -> Maybe ([RowPartition], [ColumnPartition])
parseLine line = if length parsedRow == 7 && length parsedColumn == 3
  then Just (parsedRow, parsedColumn) else Nothing

  where
    (row, column) = splitAt 7 line
    parsedRow = map fromJust . filter isJust . map parseRowPartition $ row
    parsedColumn = map fromJust . filter isJust . map parseColumnPartition $ column

partitionToSeat :: ([RowPartition], [ColumnPartition]) -> Maybe Seat
partitionToSeat = undefined

data Seat = Seat {
  row :: Int,
  column :: Int
} deriving Show

mkSeat :: ([RowPartition], [ColumnPartition]) -> Seat
mkSeat (rp, cp) = Seat {row=r, column=c}
  where
    r = rowSearch rp
    c = columnSearch cp

rowSearch :: [RowPartition] -> Int
rowSearch rows = fst $ binarySpaceSearch 128 steps
  where
    steps = map (\x -> case x of
                    Front -> LowerHalf
                    Back -> UpperHalf) rows

columnSearch :: [ColumnPartition] -> Int
columnSearch cols = fst $ binarySpaceSearch 8 steps
  where
    steps = map (\x -> case x of
                    CLeft -> LowerHalf
                    CRight -> UpperHalf) cols

data BinarySpace = UpperHalf | LowerHalf deriving Show

binarySpaceSearch :: Int -> [BinarySpace] -> (Int, Int)
binarySpaceSearch total steps = h steps 0 (total-1)
  where
    h :: [BinarySpace] -> Int -> Int -> (Int, Int)
    h sts from to
      | from == to = (from, to)
    h [] from to = (from, to)
    h (LowerHalf:sts) from to = h sts from (from+(to-from-1) `div` 2)
    h (UpperHalf:sts) from to = h sts (from+(to-from+1) `div` 2) to

seatID :: Seat -> Int
seatID Seat{row=r, column=c} = r*8 + c
