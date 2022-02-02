module Problems.Day20.Day20
  ( day20a,
    day20b,
  )
where

import           Data.Bits (shift, (.&.))
import           Data.List (foldl', transpose)
import           Data.Map  (Map, (!))
import qualified Data.Map  (fromList)
import           Data.Set  (Set, member)
import qualified Data.Set  (fromList, map)

day20a = undefined

day20b = undefined

type Algorithm = Map Int Bool

type Point = (Int, Int)

type Bounds = (Point, Point)

type Image = [[Bool]]

main = do
  contents <- lines <$> readFile "input.txt"
  let algorithm = readAlgorithm (head contents)
  let image = readImage $ drop 2 contents
  putStrLn $ "20a: " ++ show (litPointsAfter 2 algorithm image)
  putStrLn $ "20b: " ++ show (litPointsAfter 50 algorithm image)

readAlgorithm :: String -> Algorithm
readAlgorithm = Data.Map.fromList . zip [0 ..] . map (== '#')

readImage :: [String] -> Image
readImage = (map . map) (== '#')

readBinary :: [Bool] -> Int
readBinary = foldl' (\acc elem -> 2 * acc + intify elem) 0
  where
    intify b = if b then 1 else 0

calculateRow :: [Bool] -> [Int]
calculateRow = calculateRow' []

calculateRow' :: [Int] -> [Bool] -> [Int]
calculateRow' acc row
  | null row = reverse acc
  | otherwise = calculateRow' (nextNumber : acc) nextRow
  where
    nextBit = if head row then 1 else 0
    nextNumber
      | null acc = readBinary . take 3 $ row
      | otherwise = ((head acc .&. 3) `shift` 1) + nextBit
    nextRow
      | null acc = drop 3 row
      | otherwise = tail row

calculateCols :: [[Int]] -> [[Int]]
calculateCols numbers = transpose . map (calculateCol []) $ numbers'
  where
    numbers' = transpose numbers

calculateCol :: [Int] -> [Int] -> [Int]
calculateCol acc col
  | null col = reverse acc
  | otherwise = calculateCol (nextNumber : acc) nextCol
  where
    nextNumber
      | null acc = sum $ zipWith shift (take 3 col) [6, 3, 0]
      | otherwise = ((head acc .&. 63) `shift` 3) + head col
    nextCol
      | null acc = drop 3 col
      | otherwise = tail col

padImage :: a -> [[a]] -> [[a]]
padImage with image = pad (replicate m with) image'
  where
    pad with row = with : row ++ [with]
    image' = map (pad with) image
    m = length . head $ image'

enhance :: Algorithm -> (Image, Bool) -> (Image, Bool)
enhance algorithm (image, borderValue) = (image', borderValue')
  where
    padded = last . take 3 . iterate (padImage borderValue) $ image
    indices = calculateCols . map calculateRow $ padded
    image' = (map . map) (algorithm !) indices
    borderValue' =
      if borderValue
        then algorithm ! 511
        else algorithm ! 0

countLitPoints :: Image -> Int
countLitPoints image = sum . map sum $ intified
  where
    intified = (map . map) (\b -> if b then 1 else 0) image

litPointsAfter :: Int -> Algorithm -> Image -> Int
litPointsAfter steps algorithm image =
  countLitPoints . fst . last . take (steps + 1) . iterate enhance' $ (image, False)
  where
    enhance' = enhance algorithm
