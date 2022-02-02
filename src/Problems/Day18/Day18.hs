module Problems.Day18.Day18
  ( day18a,
    day18b,
  )
where

import           Data.Char  (isDigit)
import           Data.List  (foldl1', intercalate, sort)
import           Data.Maybe (fromJust, isJust)
import           Utils      (splitChar)

day18a = undefined

day18b = undefined

data Snumber = Node Int | Pair Snumber Snumber deriving (Eq)

isNode :: Snumber -> Bool
isNode (Node _) = True
isNode _        = False

isPair :: Snumber -> Bool
isPair = not . isNode

getValue :: Snumber -> Int
getValue (Node value) = value
getValue _            = error "pair has no value"

getLeft :: Snumber -> Snumber
getLeft (Pair left _) = left
getLeft _             = error "node has no left"

getRight :: Snumber -> Snumber
getRight (Pair _ right) = right
getRight _              = error "node has no right"

instance Show Snumber where
  show (Node value)      = show value
  show (Pair left right) = "[" ++ show left ++ "," ++ show right ++ "]"

(/+/) :: Snumber -> Snumber -> Snumber
(/+/) = Pair

addToValue :: Int -> Snumber -> Snumber
addToValue k (Node value) = Node (value + k)
addToValue _ split        = split

data Crumb = LeftCrumb Snumber | RightCrumb Snumber deriving (Show)

isLeftCrumb :: Crumb -> Bool
isLeftCrumb (LeftCrumb _) = True
isLeftCrumb _             = False

isRightCrumb :: Crumb -> Bool
isRightCrumb = not . isLeftCrumb

type Path = [Crumb]

type Focus = (Snumber, Path)

data Action = Explode | Split deriving (Show)

main = do
  contents <- lines <$> readFile "input.txt"
  let snumbers = map parseSnumber contents
  let result = foldl1' foldF snumbers
  putStrLn $ "18a: " ++ show (getMagnitude . reduce $ result)
  let pairs = filter (\(a, b) -> a /= b) [(a, b) | a <- snumbers, b <- snumbers]
  let magnitudes = map (\(a, b) -> getMagnitude . reduce $ a /+/ b) pairs
  putStrLn $ "18b: " ++ show (getMagnitude . reduce $ result)
  where
    foldF a b = reduce $ a /+/ b

-- Stuff for reading the input

charFrequency :: Char -> String -> Int
charFrequency c = length . filter (== c)

splitTopMost :: String -> (String, String)
splitTopMost s = (left, right)
  where
    parts = splitChar ',' s
    splitIndex = getSplitIndex parts 0 0
    left = intercalate "," (take splitIndex parts)
    right = intercalate "," (drop splitIndex parts)

getSplitIndex :: [String] -> Int -> Int -> Int
getSplitIndex parts index depth
  | null parts = error "cant find split index"
  | newDepth == 0 = index + 1
  | otherwise = getSplitIndex (tail parts) (index + 1) newDepth
  where
    sorted = sort . head $ parts
    newDepth =
      depth
        + charFrequency '[' (head parts)
        - charFrequency ']' (head parts)

parseSnumber :: String -> Snumber
parseSnumber s =
  Pair left right
  where
    (left', right') = splitTopMost . tail . init $ s
    left
      | all isDigit left' = Node (read left' :: Int)
      | otherwise = parseSnumber left'
    right
      | all isDigit right' = Node (read right' :: Int)
      | otherwise = parseSnumber right'

-- Stuff for tree traversal

traversePath :: Focus -> Path -> Maybe Focus
traversePath focus [] = Just focus
traversePath focus crumbs = do
  let crumb = last crumbs
  next <- case crumb of
    LeftCrumb _  -> goLeft focus
    RightCrumb _ -> goRight focus
  traversePath next (init crumbs)

goLeft :: Focus -> Maybe Focus
goLeft (Pair left right, path) =
  Just (left, crumb : path)
  where
    crumb = LeftCrumb right
goLeft _ = Nothing

continueLeft :: Focus -> Focus
continueLeft focus = maybe focus continueLeft (goLeft focus)

goRight :: Focus -> Maybe Focus
goRight (Pair left right, path) =
  Just (right, crumb : path)
  where
    crumb = RightCrumb left
goRight _ = Nothing

continueRight :: Focus -> Focus
continueRight focus = maybe focus continueRight (goRight focus)

goUp :: Focus -> Maybe Focus
goUp (snumber, LeftCrumb right : rest) =
  Just (Pair snumber right, rest)
goUp (snumber, RightCrumb left : rest) =
  Just (Pair left snumber, rest)
goUp (snumber, []) = Nothing

continueUpUntil :: (Focus -> Bool) -> Focus -> Maybe Focus
continueUpUntil condition focus = do
  up <- goUp focus
  if condition focus
    then return focus
    else continueUpUntil condition up

goUpToRoot :: Focus -> Snumber
goUpToRoot focus =
  case goUp focus of
    Just up -> goUpToRoot up
    Nothing -> fst focus

findLeftNeighbor :: Focus -> Maybe Focus
findLeftNeighbor focus = do
  juncture <- continueUpUntil condition focus >>= goUp >>= goLeft
  return $ continueRight juncture
  where
    condition (_, crumb : _) = isRightCrumb crumb
    condition (_, [])        = False

findRightNeighbor :: Focus -> Maybe Focus
findRightNeighbor focus = do
  juncture <- continueUpUntil condition focus >>= goUp >>= goRight
  return $ continueLeft juncture
  where
    condition (_, crumb : _) = isLeftCrumb crumb
    condition (_, [])        = False

-- Actual reduction code

getDepth :: Focus -> Int
getDepth (_, path) = length path - 1

replace :: (Snumber -> Snumber) -> Focus -> Focus
replace f (snumber, path) = (f snumber, path)

explode :: Focus -> Snumber
explode (Pair (Node leftVal) (Node rightVal), path) = goUpToRoot focus'''
  where
    -- TODO shouldn't use `fromJust`
    zero = const (Node 0)
    focus = replace zero (Pair (Node leftVal) (Node rightVal), path)
    leftNeighbor = findLeftNeighbor focus
    focus' = case leftNeighbor of
      Just left -> replace (addToValue leftVal) left
      Nothing   -> focus
    -- TODO shouldn't use `fromJust`
    focus'' = fromJust $ traversePath (goUpToRoot focus', []) path
    rightNeighbor = findRightNeighbor focus''
    focus''' = case rightNeighbor of
      Just right -> replace (addToValue rightVal) right
      Nothing    -> focus''
explode focus = error (show focus)

split :: Focus -> Snumber
split (Node value, path) =
  goUpToRoot $ replace newPair (Node value, path)
  where
    left = floor (fromIntegral value / 2)
    right = ceiling (fromIntegral value / 2)
    newPair = const (Pair (Node left) (Node right))
split _ = error "invalid split arg"

-- TODO refactor these nextAction bits

nextExplode :: Focus -> (Focus, Maybe Action)
nextExplode focus
  | isPair' && depth >= 3 && isExplodeable = (focus, Just Explode)
  | isPair' && isJust leftAction = (leftFocus, leftAction)
  | isPair' && isJust rightAction = (rightFocus, rightAction)
  | otherwise = (focus, Nothing)
  where
    snumber = fst focus
    isNode' = isNode snumber
    isPair' = not isNode'
    depth = getDepth focus
    value = getValue snumber
    (leftFocus, leftAction) = nextExplode . fromJust . goLeft $ focus
    (rightFocus, rightAction) = nextExplode . fromJust . goRight $ focus
    isExplodeable = (isNode . getLeft) snumber && (isNode . getRight) snumber

nextSplit :: Focus -> (Focus, Maybe Action)
nextSplit focus
  | isNode' && value > 9 = (focus, Just Split)
  | isPair' && isJust leftAction = (leftFocus, leftAction)
  | isPair' && isJust rightAction = (rightFocus, rightAction)
  | otherwise = (focus, Nothing)
  where
    snumber = fst focus
    isNode' = isNode snumber
    isPair' = not isNode'
    depth = getDepth focus
    value = getValue snumber
    (leftFocus, leftAction) = nextSplit . fromJust . goLeft $ focus
    (rightFocus, rightAction) = nextSplit . fromJust . goRight $ focus
    isExplodeable = (isNode . getLeft) snumber && (isNode . getRight) snumber

reduce snumber =
  case nextExplode (snumber, []) of
    (focus, Just Explode) -> reduce $ explode focus
    _ ->
      case nextSplit (snumber, []) of
        (focus, Just Split) -> reduce $ split focus
        _                   -> snumber

getMagnitude :: Snumber -> Int
getMagnitude (Node value)      = value
getMagnitude (Pair left right) = 3 * getMagnitude left + 2 * getMagnitude right
