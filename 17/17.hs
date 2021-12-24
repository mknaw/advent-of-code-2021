import           Data.Char (isDigit)
import           Data.List (iterate, nub)
import           Data.Map  (Map, findMax, findMin, keys, union)
import qualified Data.Map  (filter, fromList)
import           Data.Set  (Set, size)

import           Utils     (splitChar)

type Area = ((Int, Int), (Int, Int))
type Vector = (Int, Int)
type Range = (Int, Int)

main = do
    targetArea <- readTargetArea <$> readFile "input.txt"
    let highestY = (seriesSum . highestInitialY) targetArea
    putStrLn $ "17a: " ++ show highestY
    let yDomain = [lowestInitialY targetArea..highestInitialY targetArea]
    let yRanges =
          Data.Map.fromList $
          zip yDomain (map (getTimeRangeForY targetArea) yDomain)
    let xDomain = [1..(snd . fst) targetArea]
    let xRanges =
          Data.Map.fromList (zip xDomain (map (getTimeRangeForX targetArea) xDomain))
    let tMax = (snd . snd . findMax) yRanges
    putStrLn $ "17b: " ++ show (length . nub . concatMap (getValid xRanges yRanges) $ [1..tMax])

readTargetArea :: String -> Area
readTargetArea input = ((x1, x2), (y1, y2))
  where
    filtered = filter (\c -> isDigit c || c `elem` ['.', ',', '-']) input
    [[x1, x2], [y1, y2]] =
        map
        (map (\s -> read s :: Int) . splitChar '.')
        (splitChar ',' filtered)

seriesSum :: Int -> Int
seriesSum k = k * (k + 1) `div` 2

{- | Getting highest y' is simple - the pattern is roughly
"symmetrical" so path passes by exact y0 on the way down.
Next step will be initial y' + 1 away from y0. For the
highest y', this will hit the bottom of the target on the
first step after passing y0.
-}
highestInitialY :: Area -> Int
highestInitialY ((_, _), (yMin, _)) = -yMin - 1

lowestInitialY :: Area -> Int
lowestInitialY ((_, _), (yMin, _)) = yMin

getTimeRangeForY :: Area -> Int -> Range
getTimeRangeForY ((_, _), (yMin, yMax)) y0 =
    (ceiling $ minimum boundCandidates, floor $ maximum boundCandidates)
  where
    y0F = fromIntegral y0
    yMinF = fromIntegral yMin
    yMaxF = fromIntegral yMax
    boundCandidates = filter (> 0) [
      0.5 * (1 + 2 * y0F - sqrt(4 * (y0F ** 2) + 4 * y0F - 8 * yMinF + 1)),
      0.5 * (1 + 2 * y0F - sqrt(4 * (y0F ** 2) + 4 * y0F - 8 * yMaxF + 1)),
      0.5 * (1 + 2 * y0F + sqrt(4 * (y0F ** 2) + 4 * y0F - 8 * yMinF + 1)),
      0.5 * (1 + 2 * y0F + sqrt(4 * (y0F ** 2) + 4 * y0F - 8 * yMaxF + 1))]

getTimeRangeForX :: Area -> Int -> Range
getTimeRangeForX ((xMin, xMax), (_, _)) x0
    | null boundCandidates = (-1, -1)
    | isSaturated = (ceiling (x0F - maximum boundCandidates), -1)
    | otherwise =
        (ceiling (x0F - maximum boundCandidates), floor (x0F - minimum boundCandidates))
  where
    x0F = fromIntegral x0
    xMinF = fromIntegral xMin
    xMaxF = fromIntegral xMax
    xT = ((x0F + 1) * x0F) / 2
    boundCandidates = filter (>= 0) [
      0.5 * (-1 - sqrt(8*xT - 8 * xMinF + 1)),
      0.5 * (-1 - sqrt(8*xT - 8 * xMaxF + 1)),
      0.5 * (-1 + sqrt(8*xT - 8 * xMinF + 1)),
      0.5 * (-1 + sqrt(8*xT - 8 * xMaxF + 1))]
    isSaturated = seriesSum x0 >= xMin && seriesSum x0 <= xMax

getValid :: Map Int Range -> Map Int Range -> Int -> [Vector]
getValid xs ys t = [(x, y) | x <- validXs, y <- validYs]
  where
    validXs = getValid' t xs
    validYs = getValid' t ys

getValid' :: Int -> Map Int Range -> [Int]
getValid' t = keys . Data.Map.filter (isValid t)
  where
    isValid t (-1, _) = False
    isValid t (a, -1) = t >= a
    isValid t (a, b)  = b >= a && t >= a && t <= b
