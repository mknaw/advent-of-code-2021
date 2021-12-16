import           Data.List  (foldr1)
import           Data.Map   (Map, assocs, elems, insert, lookup)
import qualified Data.Map   (empty, fromList)
import           Data.Maybe
import           Data.Set   (Set, difference, intersection, isSubsetOf, size,
                             union)
import qualified Data.Set   (filter, fromList)

data Segment = A | B | C | D | E | F | G deriving (Eq, Ord, Show)
type Segments = Set Segment

main = do
    contents <- readFile "input.txt"
    let easyCount = sum $ map (countEasyDigits . snd . extractSegments) (lines contents)
    putStrLn $ "8a: " ++ show easyCount
    putStrLn $ "8b: " ++ show (sum $ map decodeSegments (lines contents))

extractSegments :: String -> ([Segments], [Segments])
extractSegments line = (trainingSegments, predictionSegments)
  where
    (trainingSegments', _:predictionSegments') = break (== "|") (words line)
    trainingSegments = map (Data.Set.fromList . map charToSegment) trainingSegments'
    predictionSegments = map (Data.Set.fromList . map charToSegment) predictionSegments'

charToSegment :: Char -> Segment
charToSegment c
    | c == 'a'  = A
    | c == 'b'  = B
    | c == 'c'  = C
    | c == 'd'  = D
    | c == 'e'  = E
    | c == 'f'  = F
    | c == 'g'  = G
    | otherwise = error "uhoh"

identifyEasyDigit :: Segments -> Maybe Int
identifyEasyDigit s =
    case length s of
      2 -> Just 1
      4 -> Just 4
      3 -> Just 5
      7 -> Just 8
      _ -> Nothing

countEasyDigits :: [Segments] -> Int
countEasyDigits segments = length . catMaybes $ identifyEasyDigit <$> segments

classify :: [Segments] -> Map Int Segments
classify segments = classify' segments Data.Map.empty

classify' :: [Segments] -> Map Int Segments -> Map Int Segments
classify' [] digitMap = digitMap
classify' (segments:rest) digitMap =
    case getDigit segments digitMap of
      Just d -> classify' rest updated
                  where updated = insert d segments digitMap
      _      -> classify' (rest ++ [segments]) digitMap

getDigit :: Segments -> Map Int Segments -> Maybe Int
getDigit segments digitMap
    | isZero segments digitMap = Just 0
    | size segments == 2 = Just 1
    | isTwo segments digitMap = Just 2
    | isThree segments digitMap = Just 3
    | size segments == 4 = Just 4
    | isFive segments digitMap = Just 5
    | isSix segments digitMap = Just 6
    | size segments == 3 = Just 7
    | size segments == 7 = Just 8
    | isNine segments digitMap = Just 9
    | otherwise = Nothing

isZero :: Segments -> Map Int Segments -> Bool
isZero segments digitMap =
    size segments == 6
    && seven `isSubsetOf'` segments
    -- Extra check needed to check "not in"
    && isJust four
    && not (four `isSubsetOf'` segments)
  where
    four = Data.Map.lookup 4 digitMap
    seven = Data.Map.lookup 7 digitMap

isTwo :: Segments -> Map Int Segments -> Bool
isTwo segments digitMap =
    size segments == 5 && eightMinusFive `isSubsetOf'` segments
  where
    eight = Data.Map.lookup 8 digitMap
    eightMinusFive = eight `difference'` Data.Map.lookup 5 digitMap

isThree :: Segments -> Map Int Segments -> Bool
isThree segments digitMap =
    size segments == 5 && seven `isSubsetOf'` segments
  where
    seven = Data.Map.lookup 7 digitMap

isFive :: Segments -> Map Int Segments -> Bool
isFive segments digitMap =
    size segments == 5 && fourMinusSeven `isSubsetOf'` segments
  where
    four = Data.Map.lookup 4 digitMap
    seven = Data.Map.lookup 7 digitMap
    fourMinusSeven = four `difference'` seven

isSix :: Segments -> Map Int Segments -> Bool
isSix segments digitMap =
    size segments == 6
    -- Extra check needed to check "not in"
    && isJust one
    && not (one `isSubsetOf'` segments)
  where
    one = Data.Map.lookup 1 digitMap

isNine :: Segments -> Map Int Segments -> Bool
isNine segments digitMap = size segments == 6 && four `isSubsetOf'` segments
  where
    four = Data.Map.lookup 4 digitMap

reverseLookup :: Segments -> Map Int Segments -> Int
reverseLookup segments digitMap =
    fst $ head $ filter (\(d, s) -> s == segments) (assocs digitMap)

decodeSegments :: String -> Int
decodeSegments line =
    read number :: Int
  where
    (trainingSegments, predictionSegments) = extractSegments line
    digitMap = classify trainingSegments
    digitList = map (`reverseLookup` digitMap) predictionSegments
    number = concat (show <$> digitList)

-- TODO consider liftA2 once I grok this pattern
isSubsetOf' :: Maybe Segments -> Segments -> Bool
isSubsetOf' s1 s2 =
    case s1 of
      Just s1' -> s1' `isSubsetOf` s2
      _        -> False

union' :: Maybe Segments -> Maybe Segments -> Maybe Segments
union' a b = union <$> a <*> b

intersection' :: Maybe Segments -> Maybe Segments -> Maybe Segments
intersection' a b = intersection <$> a <*> b

difference' :: Maybe Segments -> Maybe Segments -> Maybe Segments
difference' a b = difference <$> a <*> b

equals' :: Maybe Segments -> Segments -> Bool
equals' m s = case m of
                Just m' -> m' == s
                _       -> False
