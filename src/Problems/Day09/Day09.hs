module Problems.Day09.Day09
  ( day09a,
    day09b,
  )
where

import           Data.Array.Unboxed ((!))
import qualified Data.Array.Unboxed as UA
import qualified Data.Char          as C
import qualified Data.List          as L
import qualified Data.Set           as S
import           Linear.V2
import           Utils
import           Utils.Matrix

type Point = V2 Int

inputFile :: String
inputFile = buildInputPath 9

parseInput :: IO (Matrix Int, [Point])
parseInput =
  do (\m -> (m, getLowPoints m))
    . matrixify
    . map (map C.digitToInt)
    . lines
    <$> readFile inputFile

day09a :: IO Int
day09a = do
  (matrix, lowPoints) <- parseInput
  return $ sum $ (+ 1) . (matrix !) <$> lowPoints

day09b :: IO Int
day09b = do
  (matrix, lowPoints) <- parseInput
  let basinSizes = map (\p -> S.size $ getBasinPoints p matrix) lowPoints
  return $ (product . take 3 . reverse . L.sort) basinSizes

getNeighborPoints :: Point -> Matrix Int -> [Point]
getNeighborPoints (V2 x y) matrix =
  filter (\(V2 x y) -> x >= 0 && y >= 0 && x < m && y < n) (zipWith V2 xs ys)
  where
    V2 m n = snd . UA.bounds $ matrix
    xs = (x +) <$> [-1, 0, 1, 0]
    ys = (y +) <$> [0, 1, 0, -1]

getNeighborValues :: Point -> Matrix Int -> [Int]
getNeighborValues point matrix = map (matrix !) $ getNeighborPoints point matrix

getLowPoints :: Matrix Int -> [Point]
getLowPoints matrix =
  filter lowPointFilter (UA.indices matrix)
  where
    lowPointFilter point = all (> val) $ getNeighborValues point matrix
      where
        val = matrix ! point

getBasinPoints :: Point -> Matrix Int -> S.Set Point
getBasinPoints point matrix =
  getBasinPoints' basinPoints edgePoints matrix
  where
    basinPoints = S.fromList [point]
    pointFilter p = matrix ! p /= 9
    edgePoints =
      S.filter pointFilter $
        S.fromList $
          getNeighborPoints point matrix

getBasinPoints' :: S.Set Point -> S.Set Point -> Matrix Int -> S.Set Point
getBasinPoints' basinPoints edgePoints matrix =
  case S.size edgePoints of
    0 -> basinPoints
    _ -> getBasinPoints' basinPoints' edgePoints' matrix
  where
    basinPoints' = basinPoints `S.union` edgePoints
    pointFilter p = p `notElem` basinPoints' && matrix ! p /= 9
    edgePoints' =
      S.filter pointFilter $
        S.fromList $
          concatMap (`getNeighborPoints` matrix) edgePoints
