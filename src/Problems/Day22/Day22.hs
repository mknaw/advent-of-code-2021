module Problems.Day22.Day22
  ( day22a,
    day22b,
  )
where

import           Data.Ix
import qualified Data.List as L
import qualified Data.Set  as S
import           Utils     (splitChar, splitString)

day22a = undefined

day22b = undefined

type Range = (Int, Int)

data Step = Step
  { _state :: Bool,
    _xs    :: Range,
    _ys    :: Range,
    _zs    :: Range
  }
  deriving (Show)

main :: IO ()
main = do
  steps <- (readStep <$>) . lines <$> readFile "input.txt"
  let bounded = boundStepsToArea ((-50, 50), (-50, 50), (-50, 50)) steps
  putStrLn $ "22a: " ++ show (count3D bounded)
  putStrLn $ "22b: " ++ show (count3D steps)

-- Parse

pairify :: [a] -> (a, a)
pairify [a, b] = (a, b)
pairify _      = error "uhoh"

readInt :: String -> Int
readInt s = read s :: Int

readStep :: String -> Step
readStep s =
  Step
    { _state = state' == "on",
      _xs = xs,
      _ys = ys,
      _zs = zs
    }
  where
    [state', ranges'] = splitChar ' ' s
    ranges = splitChar ',' ranges'
    [xs, ys, zs] =
      map (pairify . (readInt <$>) . splitString ".." . drop 2) ranges

-- Clunky knockoff of what I should just get from `Data.Range` library

rangeUnion :: [Range] -> Range -> [Range]
rangeUnion rs r = (a, b) : rs'''
  where
    rs' = filter (\(a, b) -> not (r `inRange` a && r `inRange` b)) rs
    (containingStart, rs'') = L.partition (`inRange` fst r) rs'
    (containingEnd, rs''') = L.partition (`inRange` snd r) rs''
    a = minimum $ map fst (r : containingStart ++ containingEnd)
    b = maximum $ map snd (r : containingStart ++ containingEnd)

rangeDifference :: [Range] -> Range -> [Range]
rangeDifference rs r = concatMap clamp rs'
  where
    r' = (fst r - 1, snd r + 1)
    rs' = filter (\(a, b) -> not (r `inRange` a && r `inRange` b)) rs
    clamp (a, b)
      | r `inRange` a = [(snd r', b)]
      | r `inRange` b = [(a, fst r')]
      | (a, b) `inRange` fst r' && (a, b) `inRange` snd r' =
        [(a, fst r'), (snd r', b)]
      | otherwise = [(a, b)]

-- Filter only needed for part A

boundStepsToArea :: (Range, Range, Range) -> [Step] -> [Step]
boundStepsToArea (rX, rY, rZ) = map clamp . filter filterF
  where
    -- TODO this is UGLY
    filterF step =
      inRange rX (fst $ _xs step) || inRange rX (snd $ _xs step)
        && inRange rY (fst $ _ys step) || inRange rY (snd $ _ys step)
        && inRange rZ (fst $ _zs step) || inRange rZ (snd $ _zs step)
    clamp step =
      step
        { _xs = clamp' (_xs step) rX,
          _ys = clamp' (_ys step) rY,
          _zs = clamp' (_zs step) rZ
        }
      where
        clamp' (a1, b1) (a2, b2) = (max a1 a2, min b1 b2)

-- Slice & count

-- | Filter Steps to only those that change data in a given plane
stepsForSlice :: (Step -> Range) -> [Step] -> Int -> [Step]
stepsForSlice get steps r = filter (flip inRange r . get) steps

-- | Get count of lit points on a 1-dimentional segment
count1D :: [Step] -> Int
count1D steps = sum . map (\(a, b) -> b - a + 1) $ ranges
  where
    foldStep :: (Step -> Range) -> [Range] -> Step -> [Range]
    foldStep get rs step =
      if _state step
        then rangeUnion rs $ get step
        else rangeDifference rs $ get step

    ranges = L.foldl' (foldStep _xs) [] steps

-- | Get count of lit points on a 2-dimentional plane
count2D :: [Step] -> Int
count2D steps = countAndDrag _ys (count1D . stepsForSlice _ys steps) steps

-- | Get count of lit points in a 3-dimentional area
count3D :: [Step] -> Int
count3D steps = countAndDrag _zs (count2D . stepsForSlice _zs steps) steps

-- | Sum counts over a dimension (move, or multiply, while invariant)
countAndDrag :: (Step -> Range) -> (Int -> Int) -> [Step] -> Int
countAndDrag get f steps = sum $ zipWith (*) (pairDiffs partitionsPlus) counts
  where
    partitions = getPartition get steps
    partitionsPlus = S.toList $ S.union partitions (S.map (+ 1) partitions)
    counts = map f partitionsPlus

getPartition :: (Step -> Range) -> [Step] -> S.Set Int
getPartition get = L.foldl' fold S.empty
  where
    fold partitions step = S.union partitions $ S.fromList [a, b]
      where
        (a, b) = get step

pairDiffs :: [Int] -> [Int]
pairDiffs = pairDiffs' []

pairDiffs' :: [Int] -> [Int] -> [Int]
pairDiffs' acc (a : b : rest) = pairDiffs' (b - a : acc) (b : rest)
pairDiffs' acc _              = reverse acc
