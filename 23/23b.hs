import           Data.Fixed
import qualified Data.Maybe as M

data AmpT = A | B | C | D deriving (Eq, Ord, Show)

-- TODO "list of size n"?
type Slots = [Maybe AmpT]
type Stash = [Maybe AmpT]

maxT :: Slots -> Maybe AmpT
maxT slots
    | null slots' = Nothing
    | otherwise   = Just $ maximum slots'
  where slots' = M.catMaybes slots

idxs :: (Eq a) => a -> [a] -> [Int]
idxs a = map fst . filter ((== a) . snd) . zip [0..]

targetIdxOf :: AmpT -> Int
targetIdxOf A = 0
targetIdxOf B = 1
targetIdxOf C = 2
targetIdxOf D = 3

costMultiplierOf :: AmpT -> Int
costMultiplierOf A = 1
costMultiplierOf B = 10
costMultiplierOf C = 100
costMultiplierOf D = 1000

data Direction = L | R deriving (Show)

directionBetween :: Int -> Int -> Direction
directionBetween from to
    | from < to = R
    | from > to = L
    | otherwise = error "TODO? equal"

directionToTarget :: Slots -> Int -> Direction
directionToTarget slots i = directionBetween i t
  where
    a = M.fromJust $ slots !! i
    t = targetIdxOf a

replace :: Int -> [a] -> a -> [a]
replace index lst new = before ++ [new] ++ after
  where (before, _:after) = splitAt index lst

stashAt :: Stash -> Int -> AmpT -> Maybe Stash
stashAt stash i a =
    case stash !! i of
      Nothing -> Just $ replace i stash (Just a)
      Just _  -> Nothing

shouldStash :: Slots -> Int -> Int -> Bool
shouldStash slots from to = a `cmp` b
  where
    a = slots !! from
    b = slots !! to
    direction = directionBetween from to
    cmp = case direction of
            L -> (<)
            R -> (>)

stashIdxsBetween :: (Int, Int) -> [Int]
stashIdxsBetween (from, to)
    | from > to = stashIdxsBetween (to, from)
    | otherwise = map (from + 2 +) $ take d [0..]
  where d = to - from

getAvailableStashSpots :: (Int, Int) -> [Int]
getAvailableStashSpots p = filter (`notElem` restricted) [0..6]
  where restricted = stashIdxsBetween p

stashCost :: AmpT -> Int -> Int -> Int
stashCost a from i
    | distanceFromPath == 0 = 0
    | otherwise             = (2 * distanceFromPath - 1) * costMultiplierOf a
  where
    t = targetIdxOf a
    path = stashIdxsBetween (from, t)
    a' = minimum path
    b' = maximum path
    distanceFromPath | i < a' = a' - i
                     | i > b' = i - b'
                     | otherwise = 0

canPopFromStash :: Stash -> Slots -> Int -> Bool
canPopFromStash stash slots i = M.isNothing (slots !! i)
  where
    a = M.fromJust (stash !! i)
    t = targetIdxOf a
    -- TODO and path unobstructed...

main :: IO ()
main = do
    let slots = map Just [B, C, B, A]
    let a = maxT slots
    let i = last $ idxs a slots
    let t = targetIdxOf (M.fromJust a)
    let shouldStash' = shouldStash slots i t
    let stashSpots = getAvailableStashSpots (i, t)
    print $ map (stashCost B 2) stashSpots
