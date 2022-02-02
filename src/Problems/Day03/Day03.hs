module Problems.Day03.Day03
  ( day03a,
    day03b,
  )
where

import           Data.Char     (digitToInt)
import           Data.Foldable
import           Data.Function (on)
import           Data.List     (filter, foldl', group, maximumBy, minimumBy,
                                sort, transpose)

day03a = undefined

day03b = undefined

main = do
  contents <- readFile "input.txt"
  -- Find the `gamma` by calculating most common bit in all columns
  let gammaBinary = findGammaBinary (lines contents)
  let gamma = binaryStringToInt gammaBinary
  -- The `epsilon` is supposed to be the least common bit in all columns,
  -- but since the data is binary, we don't have to iterate again to figure it out
  let epsilon = binaryStringToInt $ negateBinaryString gammaBinary
  putStrLn $ "3a: " ++ show (gamma * epsilon)
  -- Find the "oxygen rating" by grouping filtering numbers on their bits being
  -- in the most common configuration
  let oxygenBinary = bitCriteriaSearch mostCommon (lines contents)
  let co2Binary = bitCriteriaSearch leastCommon (lines contents)
  putStrLn $ "3b: " ++ show (binaryStringToInt oxygenBinary * binaryStringToInt co2Binary)

findGammaBinary :: [String] -> String
findGammaBinary lst = map mostCommon (transpose lst)

commonAgg ::
  (Ord a1, Ord a2) =>
  -- this is either `minimumBy` or `maximumBy` for our purposes.
  -- (don't really know why has to have `Int` there but whatever)
  (((a2, a1) -> (a2, a1) -> Ordering) -> [(a2, Int)] -> (a2, a1)) ->
  [a2] ->
  a2
commonAgg _ [] = error "unexpected input"
commonAgg f list = fst $ f (compare `on` snd) counts
  where
    counts = [(head el, length el) | el <- group . sort $ list]

leastCommon :: (Eq a, Ord a) => [a] -> a
leastCommon = commonAgg minimumBy

mostCommon :: (Eq a, Ord a) => [a] -> a
mostCommon = commonAgg maximumBy

binaryStringToInt :: String -> Int
binaryStringToInt = foldl' (\acc x -> 2 * acc + digitToInt x) 0

negateBinaryString :: String -> String
negateBinaryString = map (\x -> if x == '1' then '0' else '1')

bitCriteriaSearch :: (String -> Char) -> [String] -> String
bitCriteriaSearch f = bitCriteriaSearch' f 0

bitCriteriaSearch' :: (String -> Char) -> Int -> [String] -> String
bitCriteriaSearch' f pos list
  | length candidates == 1 = head candidates
  | otherwise = bitCriteriaSearch' f (pos + 1) candidates
  where
    bit = f . map (!! pos) $ list
    candidates = filter (\s -> s !! pos == bit) list
