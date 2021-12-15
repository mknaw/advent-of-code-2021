import Data.Char (digitToInt)
import Data.List (findIndex, foldl)

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

main = do
    contents <- lines <$> readFile "input.txt"
    let matrix = map digitToInt <$> contents
    let steps = 100
    putStrLn $ "11a: " ++ show (sum $ map countFlashes $ take (steps + 1) $ iterate advance matrix)
    let matrixSize = uncurry (*) (matrixDims matrix)
    putStrLn $ "11b: " ++ show (findIndex (\m -> countFlashes m == matrixSize) (iterate advance matrix))

advance :: Matrix -> Matrix
advance = propagateLights . (map.map) (+1)

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
    xs = (x+) <$> [-1, -1, 0, 1, 1, 1, 0, -1]
    ys = (y+) <$> [0, 1, 1, 1, 0, -1, -1, -1]
    within k max = 0 <= k && k < max

replaceInList :: Int -> [a] -> a -> [a]
replaceInList index lst new = before ++ [new] ++ after
  where (before, _:after) = splitAt index lst

replaceInMatrix :: Point -> Matrix -> Int -> Matrix
replaceInMatrix (x, y) matrix new =
    replaceInList y matrix (replaceInList x (matrix !! y) new)

incrementPoint :: Point -> Matrix -> Matrix
incrementPoint point matrix = replaceInMatrix point matrix (matrix !. point + 1)

countFlashes :: Matrix -> Int
countFlashes = sum . map (length . filter (==0))
