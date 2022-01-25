import           Control.Monad  (forM, forM_)
import qualified Data.Bifunctor as B
import           Data.Bits      ((.&.), (.|.))
import qualified Data.Bits      as B
import qualified Data.List      as L
import qualified Data.Maybe     as M

data CType = E | S deriving (Show)
type Vector = (Int, Int)
data Cuke = Cuke
    { _type :: CType
    , _pos  :: Vector
    } deriving (Show)

data CukeState = CukeState
    { _cukes :: [Cuke]
    , _maxX  :: Int
    , _maxY  :: Int
    }

data Row = Row
    { _e :: Int
    , _s :: Int
    } deriving (Show)

main :: IO ()
main = do
    lines' <- lines <$> readFile "input.txt"
    let bitSize = length . head $ lines'
    let rows = map parseBinary lines'
    mapM_ (putStrLn . printRow bitSize) (rotateCCW bitSize rows)
    -- print $ map _s rows
    -- putStrLn $ printRow bitSize (column bitSize rows 0)

printCukes :: CukeState -> IO ()
printCukes cs = do
    forM_ [0.._maxY cs] $ \y -> do
        let row = map (\x -> charFor (x, y)) [0.._maxX cs]
        putStrLn row
  where
    charFor (x, y) =
        case L.find (\c -> _pos c == (x, y)) (_cukes cs) of
            Just Cuke { _type=E } -> '>'
            Just Cuke { _type=S } -> 'v'
            Nothing               -> '.'

parseLines :: [String] -> [Cuke]
parseLines ls = concatMap cukify byRow
  where
    byRow = zip [0..] $ map parseLine ls
    cukify (y, lst) = [Cuke { _type=snd p, _pos=(fst p, y) } | p <- lst]

    parseLine :: String -> [(Int, CType)]
    parseLine s = map (B.second M.fromJust)
                . filter (M.isJust . snd)
                . zip [0..] $ map parse s
      where
        parse :: Char -> Maybe CType
        parse c = case c of '>' -> Just E
                            'v' -> Just S
                            _   -> Nothing

shift :: Int -> Int
shift i = B.shiftR i 1 .|. B.shiftL end total
  where
    end = i .&. 1
    total = 5  -- TODO

move :: Int -> Int
move i = unmoved .|. moved
  where
    shifted = shift i
    moved = shifted .&. B.complement i
    unmoved = i .&. B.complement (B.shiftL 1 moved)

parseBinary :: String -> Row
parseBinary s = Row { _e=parseBinary' '>' s, _s=parseBinary' 'v' s }
  where
    parseBinary' :: Char -> String -> Int
    parseBinary' c = L.foldl' f 0
      where
        f acc c' = 2 * acc + k
            where k = if c' == c then 1 else 0

rotateCCW :: Int -> [Row] -> [Row]
rotateCCW b rows = map (\k -> L.foldr (fold k) empty rows) bitMaps
  where
    bitMaps = reverse [2 ^ k | k <- [0..b - 1]]
    empty = Row { _e=0, _s=0 }
    fold k row Row { _s=s, _e=e } = Row { _e=2*e + e', _s=2*s + s' }
      where s' = min 1 $ _s row .&. k
            e' = min 1 $ _e row .&. k

printRow :: Int -> Row -> String
printRow b Row { _e=e, _s=s } = map f bits
  where
    bits = reverse [2^k | k <- [0..b - 1]]
    f k | e .&. k /= 0 = '>'
        | s .&. k /= 0 = 'v'
        | otherwise      = '.'

column b rows i = Row { _e=0, _s=s }
  where
    k = 2 ^ (b - i - 1)
    s = L.foldl1' (.|.) . map (\row -> _s row .&. k) $ rows
