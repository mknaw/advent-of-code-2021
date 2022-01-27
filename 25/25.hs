import           Control.Monad      (forM, forM_)
import           Data.Array.IArray  ((!))
import qualified Data.Array.IArray  as IA
import qualified Data.Array.Unboxed as UA
import qualified Data.Bifunctor     as B
-- import           Data.Bits          ((.&.), (.|.))
import qualified Data.Bits          as B
import qualified Data.List          as L
import qualified Data.Maybe         as M

type RowArray = UA.UArray Int Bool

toRowArray :: [Bool] -> RowArray
toRowArray lst = IA.array (0, length lst - 1) $ zip [0..] lst

combine :: (Bool -> Bool -> Bool) -> RowArray -> RowArray -> RowArray
combine f arr1 arr2
    | (l, u) /= IA.bounds arr2 = error "uhoh"
    | otherwise = IA.array (l, u) [(i, f (arr1!i) (arr2!i)) | i <- IA.indices arr1]
  where (l, u) = IA.bounds arr1

(.|.) :: RowArray -> RowArray -> RowArray
(.|.) = combine (||)

(.&.) :: RowArray -> RowArray -> RowArray
(.&.) = combine (&&)

(.-.) :: RowArray -> RowArray -> RowArray
(.-.) = combine (\a b -> a && not b)

-- TODO ? now thinking with this array stuff that it would be
-- better to have some boxed finite 3-state thing instead of 2 matrices
-- TODO maybe quicker to operate on the matrix in place?
data State = State [RowArray] [RowArray] deriving (Eq)

instance Show State where
  show (State e s) =
      L.intercalate "\n" $ zipWith (curry showPair) e s
    where
      showPair :: (RowArray, RowArray) -> String
      showPair (e, s) = map f $ IA.indices e
        where f i | e!i = '>'
                  | s!i       = 'v'
                  | otherwise = '.'

main :: IO ()
main = do
    state <- parseState . lines <$> readFile "input.txt"
    print $ stepsUntilFixed state

arrayForChar :: Char -> String -> RowArray
arrayForChar c = toRowArray . map (== c)

parseState :: [String] -> State
parseState ls = State e s
  where e = map (arrayForChar '>') ls
        s = map (arrayForChar 'v') ls

-- TODO could extract the non `f` bits to a fn

shiftR :: RowArray -> RowArray
shiftR arr = IA.ixmap (l, u) f arr
  where
    (l, u) = IA.bounds arr
    f i | i == l    = u
        | otherwise = i - 1

shiftL :: RowArray -> RowArray
shiftL arr = IA.ixmap (l, u) f arr
  where
    (l, u) = IA.bounds arr
    f i | i == u    = l
        | otherwise = i + 1

move :: RowArray -> RowArray -> RowArray
move arr mask = unmoved .|. moved
  where
    shifted = shiftR arr
    moved = shifted .-. mask
    unmoved = arr .-. shiftL moved

-- TODO dedupe these

moveE :: State -> State
moveE (State e s) = State e' s
  where e' = zipWith (\e s -> move e (e .|. s)) e s

moveS :: State -> State
moveS (State e s) = State e s'
  where s' = zipWith (\e s -> move s (e .|. s)) e s

column :: [RowArray] -> Int -> RowArray
column arrs i = toRowArray $ map (! i) arrs

transpose :: [RowArray] -> [RowArray]
transpose arrs = map (column arrs) (IA.indices . head $ arrs)

-- TODO could make State a functor as an exercise in FP zen :^)
transposeState :: State -> State
transposeState (State e s) = State (transpose e) (transpose s)

step :: State -> State
step = transposeState . moveS . transposeState . moveE

stepsUntilFixed :: State -> Int
stepsUntilFixed = stepsUntilFixed' 1
  where
    stepsUntilFixed' :: Int -> State -> Int
    stepsUntilFixed' k state
        | state == state' = k
        | otherwise = stepsUntilFixed' (k + 1) state'
      where state' = step state
