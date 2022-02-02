module Problems.Day11.Day11
  ( day11a,
    day11b,
  )
where

import           Data.Char (digitToInt)
import           Data.List (findIndex, foldl)
import           Matrix

day11a = undefined

day11b = undefined

main = do
  contents <- lines <$> readFile "input.txt"
  let matrix = map digitToInt <$> contents
  let steps = 100
  putStrLn $ "11a: " ++ show (sum $ map countFlashes $ take (steps + 1) $ iterate advance matrix)
  let matrixSize = uncurry (*) (matrixDims matrix)
  putStrLn $ "11b: " ++ show (findIndex (\m -> countFlashes m == matrixSize) (iterate advance matrix))

advance :: Matrix -> Matrix
advance = propagateLights . (map . map) (+ 1)

isPointLit :: Point -> Matrix -> Bool
isPointLit point matrix = matrix !. point > 9

isZero :: Point -> Matrix -> Bool
isZero point matrix = matrix !. point == 0

propagateLights :: Matrix -> Matrix
propagateLights matrix =
  if null litPoints
    then matrix
    else propagateLights (propagateLight (head litPoints) matrix)
  where
    litPoints = filter (`isPointLit` matrix) (allPoints matrix)

propagateLight :: Point -> Matrix -> Matrix
propagateLight point matrix =
  foldl (flip incrementPoint) matrix' points
  where
    matrix' = replaceInMatrix point matrix 0
    neighbors = getNeighborPoints point matrix
    points = filter (\p -> not $ isZero p matrix') neighbors

getNeighborPoints :: Point -> Matrix -> [Point]
getNeighborPoints (x, y) matrix =
  filter (\(x, y) -> x `within` m && y `within` n) (zip xs ys)
  where
    (m, n) = matrixDims matrix
    xs = (x +) <$> [-1, -1, 0, 1, 1, 1, 0, -1]
    ys = (y +) <$> [0, 1, 1, 1, 0, -1, -1, -1]
    within k max = 0 <= k && k < max

incrementPoint :: Point -> Matrix -> Matrix
incrementPoint point matrix = replaceInMatrix point matrix (matrix !. point + 1)

countFlashes :: Matrix -> Int
countFlashes = sum . map (length . filter (== 0))
