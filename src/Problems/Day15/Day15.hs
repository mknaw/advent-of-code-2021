module Problems.Day15.Day15
  ( day15a,
    day15b,
  )
where

import           Data.Array.Unboxed ((!), (//))
import qualified Data.Array.Unboxed as UA
import qualified Data.Bifunctor     as B
import qualified Data.Char          as C
import           Data.Function      (on)
import qualified Data.List          as L
import qualified Data.Map           (empty, fromList, lookup)
import qualified Data.Maybe         as M
import           Matrix
import           Utils

inputFile :: String
inputFile = buildInputPath 15

type Weights = UA.Array Point Int

type Nodes = UA.Array Point Int

type Queue = [Point]

parseInput :: IO Weights
parseInput = do parse . lines <$> readFile inputFile

day15a :: IO Int
day15a = shortestPathWeight <$> parseInput

day15b :: IO Int
day15b = shortestPathWeight . (`unfold` 5) <$> parseInput

parse :: [String] -> Weights
parse ls =
  UA.array
    ((0, 0), (x - 1, y - 1))
    [ ((i, j), k) | (j, l) <- zip [0 ..] ls, (i, k) <- zip [0 ..] $ C.digitToInt <$> l
    ]
  where
    x = length . head $ ls
    y = length ls

makeNodes :: Weights -> Nodes
makeNodes w = nodes // [((0, 0), 0)] -- First one costs nothing, per instructions.
  where
    b@((lX, lY), (uX, uY)) = UA.bounds w
    nodes = UA.array b [((i, j), -1) | i <- [lX .. uX], j <- [lY .. uY]]

getNexts :: Nodes -> Point -> [Point]
getNexts nodes (x, y) =
  [ (x', y') | (x', y') <- zip (ops <*> pure x) (reverse ops <*> pure y), inRange uX x', inRange uY y', nodes ! (x', y') == -1
  ]
  where
    (uX, uY) = snd $ UA.bounds nodes
    ops = [(+ 1), id, subtract 1, id]
    inRange u k = k >= 0 && k <= u

shortestPathWeight :: Weights -> Int
shortestPathWeight w = shortestPathWeight' w nodes [(0, 0)]
  where
    nodes = makeNodes w
    p = fst . UA.bounds $ w

shortestPathWeight' :: Weights -> Nodes -> Queue -> Int
shortestPathWeight' w nodes q
  -- q'' /= L.sortBy (compare `on` sortCriterium nodes') q'' = error "problem with sort"
  | p == end = c
  | otherwise = shortestPathWeight' w nodes' q''
  where
    p = head q
    nexts = getNexts nodes p
    nodes' = updateNodes w nodes nexts c
    c = nodes' ! p
    q' = filter (`notElem` nexts) $ tail q
    q'' = L.foldl' (insertInQueue $ sortCriterium nodes') q' nexts
    end = snd . UA.bounds $ w
    hs = map (sortCriterium nodes') q''
    debug =
      L.unlines
        [ show (zip3 q'' hs $ map (nodes' !) q''),
          show nexts
        ]
        ++ "\n"

updateNodes :: Weights -> Nodes -> Queue -> Int -> Nodes
updateNodes w nodes q c = nodes // updates
  where
    updates = [(p, cmp (nodes ! p) (c + w ! p)) | p <- q]
    cmp a b
      | a == -1 = b
      | otherwise = min a b

insertInQueue :: (Point -> Int) -> Queue -> Point -> Queue
insertInQueue f q p
  | null start || null rest = L.sortBy (compare `on` f) (p : q)
  | f (last start) > f p = insertInQueue f start p ++ rest
  | f (head rest) < f p = start ++ insertInQueue f rest p
  | otherwise = start ++ [p] ++ rest
  where
    mid = fromIntegral $ length q `div` 2
    (start, rest) = splitAt mid q

sortCriterium :: Nodes -> Point -> Int
-- TODO should be able to use this for A*, but something is off - get wrong results in some cases.
-- sortCriterium nodes p@(x, y) = nodes!p - x - y
sortCriterium nodes p@(x, y) = nodes ! p

incrementWeights :: Weights -> Int -> Weights
incrementWeights w by = incrementWeight by <$> w
  where
    incrementWeight d k = (k' - 1) `rem` 9 + 1 where k' = k + d

shiftIndex :: Weights -> (Int, Int) -> Weights
shiftIndex w by = w'
  where
    (l, u) = UA.bounds w
    w' = UA.array (shift l by, shift u by) $ map (B.first (`shift` by)) $ UA.assocs w

    shift p by = B.bimap (fst p +) (snd p +) by

matrixPoints :: Int -> Int -> [(Int, Int)]
matrixPoints m n = [(i, j) | j <- [0 .. n -1], i <- [0 .. m -1]]

matrixOfIncrements :: Int -> Int -> [[Int]]
matrixOfIncrements m n = [map (j +) [0 .. m -1] | j <- [0 .. n -1]]

unfold :: Weights -> Int -> Weights
unfold w k = UA.array (lb, ub) $ concatMap UA.assocs parts
  where
    incremented = map (incrementWeights w) (concat $ matrixOfIncrements k k)
    (lb@(lx, ly), (ux, uy)) = UA.bounds w
    dx = ux - lx + 1
    dy = uy - ly + 1
    shiftBys = map (\(x, y) -> (lx + x * dx, ly + y * dy)) $ matrixPoints k k
    parts = [shiftIndex w' by | (w', by) <- zip incremented shiftBys]
    ub = B.bimap (+ (dx - 1)) (+ (dy - 1)) $ last shiftBys
