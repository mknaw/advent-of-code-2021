import           Data.Char  (digitToInt)
import           Data.List  (sort)
import           Data.Set   (Set, size, union)
import qualified Data.Set   (filter, fromList)

type Point = (Int, Int)
type Matrix = [[Int]]

main = do
    matrix <- fmap (map (map digitToInt) . lines) (readFile "input.txt")
    let lowPoints = getLowPoints matrix
    putStrLn $ "9a: " ++ show (sum $ (+1) . (`matrixLookup` matrix) <$> lowPoints)
    let basinSizes = map (\p -> size $ getBasinPoints p matrix) lowPoints
    putStrLn $ "9b: " ++ show ((product . take 3 . reverse . sort) basinSizes)

matrixDims :: Matrix -> (Int, Int)
matrixDims matrix = (length $ head matrix, length matrix)

matrixLookup :: Point -> Matrix -> Int
matrixLookup (x, y) matrix = (matrix !! y) !! x

allPoints :: Matrix -> [Point]
allPoints matrix = [(x, y) | x <- [0..(m-1)], y <- [0..(n-1)]]
  where
    (m, n) = matrixDims matrix

getNeighborPoints :: Point -> Matrix -> [Point]
getNeighborPoints (x, y) matrix =
    filter (\(x, y) -> x >= 0 && y >= 0 && x < m && y < n) (zip xs ys)
  where
    (m, n) = matrixDims matrix
    xs = (x+) <$> [-1, 0, 1, 0]
    ys = (y+) <$> [0, 1, 0, -1]

getNeighborValues :: Point -> Matrix -> [Int]
getNeighborValues point matrix = map (`matrixLookup` matrix) $ getNeighborPoints point matrix

getLowPoints :: Matrix -> [Point]
getLowPoints matrix =
    filter lowPointFilter (allPoints matrix)
  where
    lowPointFilter point = all (> val) $ getNeighborValues point matrix
      where
        val = matrixLookup point matrix

getBasinPoints :: Point -> Matrix -> Set Point
getBasinPoints point matrix =
    getBasinPoints' basinPoints edgePoints matrix
  where
    basinPoints = Data.Set.fromList [point]
    pointFilter p = matrixLookup p matrix /= 9
    edgePoints =
      Data.Set.filter pointFilter
      $ Data.Set.fromList
      $ getNeighborPoints point matrix

getBasinPoints' :: Set Point -> Set Point -> Matrix -> Set Point
getBasinPoints' basinPoints edgePoints matrix =
    case size edgePoints of
      0 -> basinPoints
      _ -> getBasinPoints' basinPoints' edgePoints' matrix
  where
    basinPoints' = basinPoints `union` edgePoints
    pointFilter p = p `notElem` basinPoints' && matrixLookup p matrix /= 9
    edgePoints' =
      Data.Set.filter pointFilter
      $ Data.Set.fromList
      $ concatMap (`getNeighborPoints` matrix) edgePoints
