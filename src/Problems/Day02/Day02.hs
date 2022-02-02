module Problems.Day02.Day02
  ( day02a,
    day02b,
  )
where

import           Data.List (foldl')
import           Utils

inputFile :: String
inputFile = buildInputPath 2

parseInput :: IO [(String, Int)]
parseInput = do map readLine . lines <$> readFile inputFile

day02a :: IO Int
day02a = uncurry (*) . sumVectors <$> parseInput

day02b :: IO Int
day02b = uncurry (*) . sumAimedVectors <$> parseInput

readLine :: String -> (String, Int)
readLine line = (direction, distance)
  where
    line' = words line
    direction = head line'
    distance = read (line' !! 1) :: Int

sumVectors :: [(String, Int)] -> (Int, Int)
sumVectors = foldr f (0, 0)
  where
    f (direction, distance) (horizontal, depth)
      | direction == "forward" = (horizontal + distance, depth)
      | direction == "up" = (horizontal, depth - distance)
      | direction == "down" = (horizontal, depth + distance)
      | otherwise = error "unexpected input"

sumAimedVectors :: [(String, Int)] -> (Int, Int)
sumAimedVectors vectors = sumAimedVectors' vectors 0 (0, 0)

sumAimedVectors' :: [(String, Int)] -> Int -> (Int, Int) -> (Int, Int)
sumAimedVectors' [] _ vectorSum = vectorSum
sumAimedVectors' vectors aim (horizontal, depth) =
  sumAimedVectors' (tail vectors) aim' vectorSum'
  where
    (direction, distance) = head vectors
    aim'
      | direction == "down" = aim + distance
      | direction == "up" = aim - distance
      | otherwise = aim
    vectorSum'
      | direction == "forward" = (horizontal + distance, depth + aim * distance)
      | otherwise = (horizontal, depth)
