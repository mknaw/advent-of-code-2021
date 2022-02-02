module Problems.Day01.Day01
  ( day01a,
    day01b,
  )
where

import           Utils

inputFile :: String
inputFile = buildInputPath 1

parseInput :: IO [Int]
parseInput = do map intify . lines <$> readFile inputFile

day01a :: IO Int
day01a = countIncreases <$> parseInput

day01b :: IO Int
day01b = countWindowed <$> parseInput

countIncreases :: (Ord a) => [a] -> Int
countIncreases [_] = 0
countIncreases (x : xs) =
  if x < head xs
    then 1 + countIncreases xs
    else countIncreases xs
countIncreases _ = 0

countWindowed :: (Num a, Ord a) => [a] -> Int
countWindowed (a : b : c : d : es) =
  if a + b + c < b + c + d
    then 1 + countWindowed (b : c : d : es)
    else countWindowed (b : c : d : es)
countWindowed _ = 0
