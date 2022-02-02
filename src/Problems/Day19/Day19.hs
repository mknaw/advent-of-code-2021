module Problems.Day19.Day19
  ( day19a,
    day19b,
  )
where

import qualified Data.Bifunctor (second)
import           Data.Char      (isDigit)
import           Data.Function  (on)
import           Data.List      (intercalate, minimumBy, nub, partition, tails)
import           Data.Map       (Map, elems, filterWithKey, findMin,
                                 partitionWithKey, (!))
import qualified Data.Map       (fromList, map, toList)
import           Data.Maybe     (fromJust, isJust, listToMaybe, mapMaybe)
import qualified Data.Set       (fromList)
import           Utils          (splitChar, splitString)

day19a = undefined

day19b = undefined

data Vector = Vector
  { x :: Int,
    y :: Int,
    z :: Int
  }
  deriving (Eq)

type Vectors = [Vector]

-- TODO naming a little sus here
type Edge = (Int, Int)

type Edges = [Edge]

type Tree = [(Edge, Vector)]

type Report = Map Int Vector

minimumSpanningTree :: Report -> Tree
minimumSpanningTree = minimumSpanningTree' []

minimumSpanningTree' :: Tree -> Report -> Tree
minimumSpanningTree' edges report
  | null remaining = edges
  | otherwise = minimumSpanningTree' (newEdge : edges) report
  where
    treePoints = Data.Set.fromList $ concatMap ((\(a, b) -> [a, b]) . fst) edges
    (inTree, remaining) = partitionWithKey (\k _ -> k `elem` treePoints) report
    (pointIdx, point) = findMin remaining
    pool =
      if null inTree
        then filterWithKey (\k _ -> k /= pointIdx) report
        else inTree
    distances = Data.Map.toList $ Data.Map.map (point <->) pool
    closestPointIdx = fst . minimumBy (compare `on` snd) $ distances
    closestPoint = report ! closestPointIdx
    newEdge = ((pointIdx, closestPointIdx), point - closestPoint)

type Orientation = (Rotation, Vector)

zeroOrientation :: Orientation
zeroOrientation = (head allRotations, Vector {x = 0, y = 0, z = 0})

data Scanner = Scanner
  { _id           :: Int,
    _report       :: Report,
    _mstRotations :: [(Rotation, Vectors)],
    _orientation  :: Maybe Orientation
  }

points :: Scanner -> Vectors
points = elems . _report

originalMst :: Scanner -> Vectors
originalMst = snd . head . _mstRotations

initializeScanner :: Int -> Report -> Scanner
initializeScanner id report =
  Scanner
    { _id = id,
      _report = report,
      _mstRotations = mstRotations,
      _orientation = Nothing
    }
  where
    mst = minimumSpanningTree report
    mstRotations = collectRotations (map snd mst)

assignOrientation :: Scanner -> Orientation -> Scanner
assignOrientation scanner orientation =
  Scanner
    { _id = _id scanner,
      _report = report,
      _mstRotations = [(fst zeroOrientation, map snd . minimumSpanningTree $ report)],
      _orientation = Just orientation
    }
  where
    report = normalizeReport (_report scanner) orientation

normalizeReport :: Report -> Orientation -> Report
normalizeReport report (rotation, offset) =
  Data.Map.fromList (zip [0 ..] normalizedPoints)
  where
    normalizedPoints =
      applyOffset offset . applyRotationToVectors rotation . elems $ report

isNormalized :: Scanner -> Bool
isNormalized = isJust . _orientation

instance Show Vector where
  show vector = "(" ++ inner ++ ")"
    where
      inner = intercalate "," . map (show . (\a -> a vector)) $ [x, y, z]

instance Num Vector where
  abs _ = error "don't need"
  signum _ = error "don't need"
  fromInteger _ = error "don't need"
  (*) _ _ = error "don't need"
  (+) a b =
    Vector
      { x = x b + x a,
        y = y b + y a,
        z = z b + z a
      }
  negate a =
    Vector
      { x = - x a,
        y = - y a,
        z = - z a
      }

collectOffsets :: Vectors -> Vectors -> Vectors
collectOffsets as bs = [a - b | a <- as, b <- bs]

applyOffset :: Vector -> Vectors -> Vectors
applyOffset vector = map (vector +)

-- Equality for distance vectors - don't care about direction
(>=<) :: Vector -> Vector -> Bool
(>=<) a b
  | a == flip b = True
  | otherwise = a == b
  where
    flip v = Vector {x = - x v, y = - y v, z = - z v}

-- Distance between two points
(<->) :: Vector -> Vector -> Int
(<->) a b = sum . map abs $ [x a - x b, y a - y b, z a - z b]

type Rotation = (Vector -> Int, Vector -> Int, Vector -> Int)

-- Probably nicer to have these be actual matrices but I don't
-- feel like implementing (even a simplified) dot product.
allRotations :: [Rotation]
allRotations =
  [ (x, y, z),
    (x, negate . z, y),
    (x, negate . y, negate . z),
    (x, z, negate . y),
    (negate . x, negate . y, z),
    (negate . x, negate . z, negate . y),
    (negate . x, y, negate . z),
    (negate . x, z, y),
    (y, negate . x, z),
    (y, negate . z, negate . x),
    (y, x, negate . z),
    (y, z, x),
    (negate . y, negate . x, negate . z),
    (negate . y, negate . z, x),
    (negate . y, x, z),
    (negate . y, z, negate . x),
    (z, negate . x, negate . y),
    (z, negate . y, x),
    (z, x, y),
    (z, y, negate . x),
    (negate . z, negate . x, y),
    (negate . z, negate . y, negate . x),
    (negate . z, x, negate . y),
    (negate . z, y, x)
  ]

collectRotations :: [Vector] -> [(Rotation, Vectors)]
collectRotations vectors =
  [(rot, map (applyRotation rot) vectors) | rot <- allRotations]

applyRotation :: Rotation -> Vector -> Vector
applyRotation (a, b, g) p = Vector {x = a p, y = b p, z = g p}

applyRotationToVectors :: Rotation -> Vectors -> Vectors
applyRotationToVectors rot = map (applyRotation rot)

countShared :: Vectors -> Vectors -> Int
countShared as = sum . map (countOne as)
  where
    countOne vectors vector = if vector `elem` vectors then 1 else 0

countSharedDeltas :: Vectors -> Vectors -> Int
countSharedDeltas as = sum . map (countOne as)
  where
    countOne vectors vector = if any (>=< vector) vectors then 1 else 0

main = do
  (first : rest) <- map parseScanner . splitString "\n\n" <$> readFile "input.txt"
  let scanners = assignOrientation first zeroOrientation : rest
  let aligned = alignScanners scanners
  putStrLn $ "19a: " ++ show (length . nub . concatMap points $ aligned)
  let positions = map (snd . fromJust . _orientation) aligned
  putStrLn $ "19b: " ++ show (maxManhattan positions)

parseScanner :: String -> Scanner
parseScanner input = initializeScanner id report
  where
    vectorify [x, y, z] =
      Vector
        { x = read x :: Int,
          y = read y :: Int,
          z = read z :: Int
        }
    vectorify _ = error "uhoh"

    parseScannerReportLine :: String -> Maybe Vector
    parseScannerReportLine ('-' : '-' : _) = Nothing
    parseScannerReportLine line = Just . vectorify . splitChar ',' $ line

    id = read (filter isDigit . head . lines $ input) :: Int
    points = mapMaybe parseScannerReportLine . lines $ input
    report = Data.Map.fromList $ zip [0 ..] points

-- TODO fine to return [Rotation]
validRotations :: Scanner -> Scanner -> [(Rotation, Vectors)]
validRotations a b =
  map fst . filter ((> 0) . snd) $ zip (_mstRotations b) sharedDeltaCount
  where
    -- TODO should calculate these once beforehand and be done with it
    aMst = originalMst a
    bMsts = _mstRotations b
    sharedDeltaCount = map (countSharedDeltas aMst . snd) bMsts

validOffset :: Vectors -> Vectors -> Maybe Vector
validOffset as bs = listToMaybe candidates
  where
    offsets = collectOffsets as bs
    bs' = map (\offset -> map (+ offset) bs) offsets
    sharedCounts = map (countShared as) bs'
    candidates = map fst . filter ((>= 12) . snd) $ zip offsets sharedCounts

pairScanners :: Scanner -> Scanner -> Scanner
pairScanners a b =
  if null orientations
    then b
    else assignOrientation b $ head orientations
  where
    catZippedMaybe :: [(a, Maybe b)] -> [(a, b)]
    catZippedMaybe = map (Data.Bifunctor.second fromJust) . filter (isJust . snd)

    rotations = map fst $ validRotations a b
    preOffset = map (`applyRotationToVectors` points b) rotations
    offsets = map (validOffset (points a)) preOffset
    orientations = catZippedMaybe $ zip rotations offsets

alignScanners :: [Scanner] -> [Scanner]
alignScanners = alignScanners' []

alignScanners' :: [Int] -> [Scanner] -> [Scanner]
alignScanners' tried scanners
  | null toDo = scanners
  | otherwise = alignScanners' (_id reference : tried) next
  where
    (done, toDo) = partition isNormalized scanners
    reference = head . filter (\s -> _id s `notElem` tried) $ done
    next = done ++ map (pairScanners reference) toDo

maxManhattan :: Vectors -> Int
maxManhattan vs = maximum [a <-> b | a <- vs, b <- vs]
