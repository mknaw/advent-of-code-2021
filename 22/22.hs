import           Utils (splitChar, splitString)

data CubeState = On | Off deriving (Show)
type Range = (Int, Int)
data Step = Step
    { _state :: CubeState
    , _xs    :: Range
    , _ys    :: Range
    , _zs    :: Range
    } deriving (Show)

main :: IO ()
main = do
    steps <- (readStep <$>) . lines <$> readFile "input.txt"
    print steps

-- Parse

pairify :: [a] -> (a, a)
pairify [a, b] = (a, b)
pairify _      = error "uhoh"

readInt :: String -> Int
readInt s = read s :: Int

readStep :: String -> Step
readStep s = Step { _state=if state' == "on" then On else Off
                  , _xs=xs
                  , _ys=ys
                  , _zs=zs
                  }
  where
    [state', ranges'] = splitChar ' ' s
    ranges = splitChar ',' ranges'
    [xs, ys, zs] =
        map (pairify . (readInt <$>) . splitString ".." . drop 2) ranges
