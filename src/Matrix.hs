module Matrix
    ( Point
    , Matrix
    , (!.)
    , matrixDims
    , allPoints
    , replaceInMatrix
    ) where

type Point = (Int, Int)
type Matrix = [[Int]]

(!.) :: Matrix -> Point -> Int
matrix !. (x, y) = (matrix !! y) !! x

matrixDims :: Matrix -> (Int, Int)
matrixDims matrix = (length $ head matrix, length matrix)

allPoints :: Matrix -> [Point]
allPoints matrix = [(x, y) | x <- [0..(m-1)], y <- [0..(n-1)]]
  where
    (m, n) = matrixDims matrix

replaceInMatrix :: Point -> Matrix -> Int -> Matrix
replaceInMatrix (x, y) matrix new =
    replaceInList y matrix (replaceInList x (matrix !! y) new)
  where
    replaceInList :: Int -> [a] -> a -> [a]
    replaceInList index lst new = before ++ [new] ++ after
      where (before, _:after) = splitAt index lst


