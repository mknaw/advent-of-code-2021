import           Control.Monad  (forM, forM_)
import qualified Data.Bifunctor as B
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

main :: IO ()
main = do
    lines' <- lines <$> readFile "input.txt"
    let maxX = length $ head lines'
    let maxY = length lines'
    let state = CukeState { _cukes=parseLines lines'
                          , _maxX=maxX
                          , _maxY=maxY
                          }
    printCukes state

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
        parse c = case c of
                    '>' -> Just E
                    'v' -> Just S
                    _   -> Nothing

