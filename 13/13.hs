import           Data.Char  (digitToInt)
import           Data.List  (intercalate, isPrefixOf, foldl')
import           Data.Maybe (mapMaybe)
import           Data.Set   (Set, member, size)
import qualified Data.Set   (fromList, map)

import           Utils      (splitChar)


type Point = (Int, Int)
type Points = Set Point

data Fold = XFold Int | YFold Int deriving (Show)


main = do
    contents <- lines <$> readFile "input.txt"
    let points = Data.Set.fromList $ mapMaybe readPoint contents
    let folds = mapMaybe readFold contents
    putStrLn $ "13a: " ++ show (size $ applyFold points (head folds))
    let points' = foldl' applyFold points folds
    putStrLn "13b:"
    putStr $ getCode points'

toInt :: String -> Int
toInt a = read a :: Int

readPoint :: String -> Maybe Point
readPoint line =
    case split of
      [x, y] -> Just (toInt x, toInt y)
      _      -> Nothing
  where
    split = splitChar ',' line

readFold :: String -> Maybe Fold
readFold line
    | "fold along " `isPrefixOf` line = Just fold
    | otherwise = Nothing
  where
    line' = drop (length "fold along ") line
    rest = toInt $ drop 2 line'
    fold = case head line' of
             'x' -> XFold rest
             'y' -> YFold rest
             _   -> error "uhoh"

applyFold :: Points -> Fold -> Points
applyFold points fold =
    case fold of
      XFold x -> foldOverX x points
      YFold y -> foldOverY y points

foldOverX :: Int -> Points -> Points
foldOverX x = Data.Set.map (foldOver x)
  where
    foldOver x' (x, y)
        | x > x'    = (2 * x' - x, y)
        | otherwise = (x, y)

foldOverY :: Int -> Points -> Points
foldOverY y = Data.Set.map (foldOver y)
  where
    foldOver y' (x, y)
        | y > y'    = (x, 2 * y' - y)
        | otherwise = (x, y)

getCode :: Points -> String
getCode points =
    intercalate "\n" $ map (\y -> [printChar (x, y) | x <- [0..maxX]]) [0..maxY]
  where
    maxX = (maximum . Data.Set.map fst) points
    maxY = (maximum . Data.Set.map snd) points
    printChar point = if point `member` points then '#' else ' '
