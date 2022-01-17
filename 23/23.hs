import           Data.Function (on)
import qualified Data.List     as L
import qualified Data.Map      as M
import           Data.Maybe    (mapMaybe)
import qualified Data.Maybe    as M
import qualified Data.Set      as S

type Position = Int
-- 0  1  2  3  4  5  6  7  8  9  10
--       12    14    16    18
--       22    24    26    28

data Move = Move
    { _move         :: (Position, Position)
    , _amphipodType :: AmphipodType
    } deriving (Show)

data AmphipodType = A | B | C | D deriving (Eq, Ord, Show)
data Amphipod = Amphipod
    { _type     :: AmphipodType
    , _hasMoved :: Bool
    , _position :: Position
    } deriving (Show)

data State = State
    { _amphipods :: [Amphipod]
    , _moves     :: [Move]
    } deriving (Show)

-- new amphipod, who this?
newAmphipod :: Position -> Char -> Amphipod
newAmphipod position type' =
    Amphipod { _type=amphipodType
             , _hasMoved=False
             , _position=position }
  where
    amphipodType = case type' of
                     'A' -> A
                     'B' -> B
                     'C' -> C
                     'D' -> D
                     _   -> error "uhoh"

targetPositions :: [(AmphipodType, [Position])]
targetPositions = [
    -- Use relies on back of room being first
    (A, [22, 12]),
    (B, [24, 14]),
    (C, [26, 16]),
    (D, [28, 18]) ]

getTargetPositions :: Amphipod -> [Position]
getTargetPositions a = M.fromJust $ lookup (_type a) targetPositions

restrictedPositions :: [Position]
restrictedPositions = [2, 4, 6, 8]

excludeRestricted :: [Position] -> [Position]
excludeRestricted = filter (`notElem` restrictedPositions)

isDone :: Amphipod -> Bool
isDone a = _position a `elem` getTargetPositions a

areDone :: [Amphipod] -> Bool
areDone = all isDone

atPosition :: Position -> [Amphipod] -> Maybe Amphipod
atPosition p = L.find (\a -> _position a == p)

isOccupied :: [Amphipod] -> Position -> Bool
isOccupied as p = M.isJust $ atPosition p as

isUnoccupied :: [Amphipod] -> Position -> Bool
isUnoccupied as p = not $ isOccupied as p

getLefts :: [Amphipod] -> Position -> [Position]
getLefts as position = takeWhile (isUnoccupied as) $ reverse [0..first]
  where first = position `rem` 10

getRights :: [Amphipod] -> Position -> [Position]
getRights as position = takeWhile (isUnoccupied as) [first..10]
  where first = position `rem` 10

validHallwayPositions :: [Amphipod] -> Amphipod -> [Position]
validHallwayPositions as a = excludeRestricted (lefts ++ rights)
  where
    lefts = getLefts as (_position a)
    rights = getRights as (_position a)

isInBackOfRoom :: Amphipod -> Bool
isInBackOfRoom a = _position a > 20

isStuckInRoom :: [Amphipod] -> Amphipod -> Bool
isStuckInRoom as a = isInBackOfRoom a && isOccupied as (p - 10)
  where p = _position a

nextAmphipodsToExit :: [Amphipod] -> [Amphipod]
nextAmphipodsToExit as = validAmphipods
  where
    rooms = map snd targetPositions
    validAmphipods =
        filter (\a -> not $ isDone a && roomIsReady as a)
        . concatMap (take 1 . reverse . mapMaybe (`atPosition` as)) $ rooms

isInHallway :: Amphipod -> Bool
isInHallway a = _position a `elem` [1,2,3,4,5,6,7]

isStuckInHallway :: [Amphipod] -> Amphipod -> Bool
isStuckInHallway as a = any (isOccupied as) (pathToTarget as a)

roomIsReady :: [Amphipod] -> Amphipod -> Bool
roomIsReady as a =
    all ((== _type a) . _type) $ mapMaybe (`atPosition` as) (getTargetPositions a)

rangeBetween :: Int -> Int -> [Int]
rangeBetween a b
    | a <= b = [a..b]
    | otherwise = [b..a]

pathBetween :: Position -> Position -> [Position]
pathBetween a b = L.nub (b : p)
  where
    p = filter (/= a) $ pathBetween' a b

pathBetween' :: Position -> Position -> [Position]
pathBetween' a b
    | b <= 10 && a <= 10 = error (show "not what I had in mind")
    | a > b = reverse (pathBetween' b a)
    | otherwise = hallway ++ roomBit
  where
    a' = if a == 10 then 10 else a `rem` 10
    b' = if b == 10 then 10 else b `rem` 10
    hallway = rangeBetween a' b'
    roomBit = takeWhile (> 10) . iterate (\x -> x - 10) $ b

getDestination :: [Amphipod] -> Amphipod -> Int
getDestination as a
    | null t = error (show "here?")
    | otherwise = head t
  where t = filter (isUnoccupied as) (getTargetPositions a)

pathToTarget :: [Amphipod] -> Amphipod -> [Position]
pathToTarget as a
    | isDone a = []
    | otherwise = pathBetween p last
  where
    p = _position a
    last = getDestination as a

hasClearPathToTarget :: [Amphipod] -> Amphipod -> Bool
hasClearPathToTarget as a
    | not $ roomIsReady as a = False
    | otherwise = all (isUnoccupied as) $ pathToTarget as a

validMoves :: [Amphipod] -> Amphipod -> [Move]
validMoves as a
    | (_hasMoved a || isInBackOfRoom a) && isDone a = []
    | _hasMoved a = if isStuckInHallway as a
                       then []
                       else map wrapInMove $ take 1 validTargets
    | otherwise = if isStuckInRoom as a
                     then []
                     else map wrapInMove (validHallwayPositions as a)
  where
    validTargets = filter (isUnoccupied as) (getTargetPositions a)
    wrapInMove position = Move { _move=(_position a, position)
                               , _amphipodType=_type a }

finishingMoves :: [Amphipod] -> [Move]
finishingMoves as = map makeMove as'
  where
    as' = filter (\a -> isInHallway a && roomIsReady as a) as

    makeMove a = Move { _move=(_position a, getDestination as a)
                      , _amphipodType=_type a}

nextClearPathMove :: [Amphipod] -> Maybe Move
nextClearPathMove as
    | null as' = Nothing
    | otherwise = Just (Move { _move=(start, end), _amphipodType=_type a })
  where
    as' = filter (hasClearPathToTarget as) (filter (not . isDone) as)
    a = head as'
    start = _position a
    end = getDestination as a

moveAmphipod :: [Amphipod] -> Move -> [Amphipod]
moveAmphipod as m = a' : rest
  where
    ([a], rest) = L.partition (\a -> _position a == fst (_move m)) as
    a' = a { _position=snd (_move m), _hasMoved=True }

advance :: State -> [State]
advance State { _amphipods=as, _moves=oldMoves }
    | M.isJust clearPathMove = [makeState as (M.fromJust clearPathMove)]
    | otherwise = map (makeState as) moves
  where
    clearPathMove = nextClearPathMove as
    moves = concatMap (validMoves as) (nextAmphipodsToExit as)
    makeState as move = State
        { _moves=move : oldMoves
        , _amphipods=moveAmphipod as move
        }

simulate :: State -> [State]
simulate state = done ++ concatMap simulate todo
  where
    next = advance state
    (done, todo) = L.partition (areDone . _amphipods) next

distance :: (Position, Position) -> Int
distance (a, b) = length (pathBetween a b)

score :: State -> Int
score state = sum $ map (\m -> cost (_amphipodType m) * distance (_move m)) moves
  where
      moves = _moves state
      cost aType = M.fromJust $ lookup aType [
        (A, 1),
        (B, 10),
        (C, 100),
        (D, 1000) ]

main :: IO ()
main = do
    print $ minimum . map score . simulate $ state
  where
    amphipods :: [Amphipod]
    amphipods = [
        newAmphipod 12 'C',
        newAmphipod 14 'C',
        newAmphipod 16 'B',
        newAmphipod 18 'D',
        newAmphipod 22 'D',
        newAmphipod 24 'A',
        newAmphipod 26 'B',
        newAmphipod 28 'A' ]

    state :: State
    state = State
        { _moves=[]
        , _amphipods=amphipods
        }
