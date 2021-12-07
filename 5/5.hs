import           Data.Char (digitToInt, isDigit)
import           Data.List (group, sort)

data Point = Point
    { x :: Integer
    , y :: Integer
    }

instance Eq Point where
    (==) a b = x a == x b && y a == y b

instance Ord Point where
    compare a b
      | y a > y b = GT
      | y a < y b = LT
      | x a > x b = GT
      | x a < x b = LT
      | otherwise = EQ

instance Show Point where
    show p = "(" ++ show (x p) ++ ", " ++ show (y p) ++ ")"

newtype Segment = Segment (Point, Point)

main = do
    contents <- readFile "input.txt"
    -- Find horizontal & vertical "segments" and determine which points visited
    -- multiple times
    let segments = filter isOrthogonal $ readSegmentFromLine <$> lines contents
    let visitedPoints = concat $ getVisitedPoints <$> segments
    let sortedPoints = sort visitedPoints
    let repeatedPoints = head <$> filter (\ps -> length ps > 1) (group sortedPoints)
    putStrLn $ "5a: " ++ show (length repeatedPoints)

    -- 5b - add diagonal segments as well.
    -- It turns out this variation is actually simpler with the chosen approach.
    let segments = readSegmentFromLine <$> lines contents
    let visitedPoints = concat $ getVisitedPoints <$> segments
    let sortedPoints = sort visitedPoints
    let repeatedPoints = head <$> filter (\ps -> length ps > 1) (group sortedPoints)
    putStrLn $ "5b: " ++ show (length repeatedPoints)

readIntegersFromLine :: String -> [Integer]
readIntegersFromLine "" = []
readIntegersFromLine s = (read k :: Integer) : readIntegersFromLine rest
  where
    s' = dropWhile (not . isDigit) s
    (k, rest) = span isDigit s'

readSegmentFromLine :: String -> Segment
readSegmentFromLine l =
    Segment (Point { x=x1, y=y1 }, Point { x=x2, y=y2 })
  where
    [x1, y1, x2, y2] = readIntegersFromLine l

isOrthogonal :: Segment -> Bool
isOrthogonal (Segment (a, b))
    | x a == x b = True
    | y a == y b = True
    | otherwise = False

getVisitedPoints :: Segment -> [Point]
getVisitedPoints (Segment (a, b))
    | a == b = [b]
    | otherwise = a : getVisitedPoints (Segment (a', b))
  where
    dx = x b - x a
    dy = y b - y a
    x' | dx == 0   = x a
       | dx < 0    = x a - 1
       | otherwise = x a + 1
    y' | dy == 0   = y a
       | dy < 0    = y a - 1
       | otherwise = y a + 1
    a' = Point { x=x', y=y' }
