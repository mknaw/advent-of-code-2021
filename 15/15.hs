import           Data.Char     (digitToInt)
import           Data.Function (on)
import           Data.List     (minimumBy, transpose)
import           Data.Map      ((!), Map, keysSet, unionWith)
import qualified Data.Map      (empty, fromList, lookup)
import           Data.Maybe    (isJust, fromJust)  -- TODO think this is an antipattern
import           Data.Set      (Set, intersection, toList)
import qualified Data.Set      (delete, empty, fromList)

import           Matrix

type Distances = Map Point Int

main = do
    contents <- lines <$> readFile "input.txt"
    let weights = map digitToInt <$> contents
    putStrLn $ "15a: " ++ show (shortestPathLength weights)
    -- TODO broken for some reason. and slow. not acceptable!
    putStrLn $ "15b: " ++ show (shortestPathLength $ assembleBigMatrix weights)

getNeighbors :: Point -> Matrix -> [Point]
getNeighbors (x, y) matrix =
    filter (\(x, y) -> x `within` m && y `within` n) (zip xs ys)
  where
    (m, n) = matrixDims matrix
    -- There is no way in this problem for a shortest path to go left or up.
    xs = (x+) <$> [1, 0]
    ys = (y+) <$> [0, 1]
    within k max = 0 <= k && k < max

dijkstra :: Point -> Matrix -> Distances -> Set Point -> Distances
dijkstra point weights distances unvisited
    | unvisited' == Data.Set.empty = updated
    | proposals == Data.Map.empty = updated
    | otherwise =
        case popClosest (intersection (keysSet updated) unvisited') updated of
          Just next -> dijkstra next weights updated unvisited'
          _         -> updated
  where
    d0 = distances ! point
    propose point' = (point', d0 + weights !. point')
    proposals = Data.Map.fromList $ map propose $ getNeighbors point weights
    updated = unionWith (\a b -> minimum [a, b]) proposals distances
    unvisited' = Data.Set.delete point unvisited

popClosest :: Set Point -> Distances -> Maybe Point
popClosest unvisited distances =
    if null zipped
       then Nothing
       else Just $ fst $ minimumBy (compare `on` fromJust . snd) zipped
  where
    unvisited' = toList unvisited
    zipped = filter (isJust . snd) $ zip unvisited' (map (`Data.Map.lookup` distances) unvisited')

shortestPathLength :: Matrix -> Int
shortestPathLength weights = distances ! (m - 1,  n - 1)
  where
    (m, n) = matrixDims weights
    initialDistances = Data.Map.fromList [((0, 0), 0)]
    unvisited = Data.Set.fromList $ allPoints weights
    distances = dijkstra (0, 0) weights initialDistances unvisited

incrementMatrix :: Matrix -> Matrix
incrementMatrix = (map . map) f
  where f x = if x == 9 then 1 else x + 1

assembleBigMatrix :: Matrix -> Matrix
assembleBigMatrix matrix =
    transpose $ concatMap transpose columns
  where
    makeColumn k = concat $ take 5 $ drop k $ iterate incrementMatrix matrix
    columns = map makeColumn [0..4]
