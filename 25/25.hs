import           Control.Monad  (forM, forM_)
import qualified Data.Bifunctor as B
import           Data.Bits      ((.&.), (.|.))
import qualified Data.Bits      as B
import qualified Data.List      as L
import qualified Data.Maybe     as M

data Row = Row
    { _e :: Int
    , _s :: Int
    } deriving (Eq)

data State = State
    { _rows :: [Row]
    , _x    :: Int
    , _y    :: Int
    } deriving (Eq)

instance Show State where
  show State { _rows=rows, _x=x } = "\n" `L.intercalate` map showRow rows
    where
      showRow :: Row -> String
      showRow Row { _e=e, _s=s } = map f bits
        where
          bits = reverse [2^k | k <- [0..x - 1]]
          f k | e .&. k /= 0 = '>'
              | s .&. k /= 0 = 'v'
              | otherwise    = '.'

newState :: Int -> [Row] -> State
newState x rows = State { _rows=rows, _x=x, _y=length rows }

main :: IO ()
main = do
    state <- parseState . lines <$> readFile "input.txt"
    print $ stepsUntilFixed state

parseState :: [String] -> State
parseState ls = newState x rows
  where rows = map parseBinary ls
        x = length . head $ ls

parseBinary :: String -> Row
parseBinary s = Row { _e=parseBinary' '>' s, _s=parseBinary' 'v' s }
  where
    parseBinary' :: Char -> String -> Int
    parseBinary' c = L.foldl' f 0
      where
        f acc c' = 2 * acc + k
            where k = if c' == c then 1 else 0

shiftR :: Int -> Int -> Int
shiftR b i = B.shiftR i 1 .|. B.shiftL end (b - 1)
  where
    end = i .&. 1

shiftL :: Int -> Int -> Int
shiftL b i = (B.shiftL i 1 .|. start) `B.clearBit` b
  where
    k = 2 ^ (b - 1)
    start = min 1 $ i .&. k

move :: Int -> Int -> Int -> Int
move b i mask = unmoved .|. moved
  where
    shifted = shiftR b i
    moved = shifted .&. B.complement mask
    unmoved = i .&. B.complement (shiftL b moved)

-- TODO dedupe these
moveE :: State -> State
moveE State { _rows=rows, _x=x } = newState x rows'
  where
    rows' = map f rows
    f row@Row{ _e=e, _s=s } = row { _e=move x e (e .|. s) }

moveS :: State -> State
moveS State { _rows=rows, _x=x } = newState x rows'
  where
    rows' = map f rows
    f row@Row{ _e=e, _s=s } = row { _s=move x s (s .|. e) }

-- TODO dedupe these
rotateCCW :: State -> State
rotateCCW State { _rows=rows, _x=x, _y=y } = newState y rows'
  where
    bitMaps = [2 ^ k | k <- [0..x - 1]]
    empty = Row { _e=0, _s=0 }
    fold k row Row { _s=s, _e=e } = Row { _e=2*e + e', _s=2*s + s' }
      where s' = min 1 $ _s row .&. k
            e' = min 1 $ _e row .&. k
    rows' = map (\k -> L.foldr (fold k) empty (reverse rows)) bitMaps

rotateCW :: State -> State
rotateCW State { _rows=rows, _x=x, _y=y } = newState y rows'
  where
    bitMaps = reverse [2 ^ k | k <- [0..x - 1]]
    empty = Row { _e=0, _s=0 }
    fold k row Row { _s=s, _e=e } = Row { _e=2*e + e', _s=2*s + s' }
      where s' = min 1 $ _s row .&. k
            e' = min 1 $ _e row .&. k
    rows' = map (\k -> L.foldr (fold k) empty rows) bitMaps

step :: State -> State
step = rotateCW . moveS . rotateCCW . moveE

stepsUntilFixed :: State -> Int
stepsUntilFixed = stepsUntilFixed' 1
  where
    stepsUntilFixed' :: Int -> State -> Int
    stepsUntilFixed' k state
        | state == state' = k
        | otherwise = stepsUntilFixed' (k + 1) state'
      where state' = step state
