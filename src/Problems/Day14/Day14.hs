module Problems.Day14.Day14
  ( day14a,
    day14b,
  )
where

import           Data.List  (iterate)
import           Data.Map   (Map, elems, foldrWithKey, insert)
import qualified Data.Map   (empty, lookup)
import           Data.Maybe (fromMaybe)
import           Utils      (splitChar)

day14a = undefined

day14b = undefined

type Rules = Map String [String]

type Counter = Map String Int

type Frequencies = Map Char Int

main = do
  contents <- lines <$> readFile "input.txt"
  let polymerTemplate = head contents
  let rules = getRules (drop 2 contents)
  let counter = initializeCounter polymerTemplate
  putStrLn $ "14a: " ++ show (frequentMinusRareAfter polymerTemplate 10 rules counter)
  putStrLn $ "14b: " ++ show (frequentMinusRareAfter polymerTemplate 40 rules counter)

-- Stuff needed for initial conditions

pairify :: String -> (String, String)
pairify s = (from, to)
  where
    [from, _, to] = splitChar ' ' s

getRules :: [String] -> Rules
getRules = foldr collect Data.Map.empty
  where
    collect line rules = insert from to rules
      where
        (from, to' : _) = pairify line
        to = [head from : [to'], to' : tail from]

initializeCounter :: String -> Counter
initializeCounter template =
  foldr (`insertAdd` 1) Data.Map.empty pairs
  where
    pairs = pairWindows template

pairWindows :: String -> [String]
pairWindows (a : b : rest) = (a : [b]) : pairWindows (b : rest)
pairWindows _              = []

-- Stuff needed to run the simulation

advance :: Rules -> Counter -> Counter
advance rules = foldrWithKey fold Data.Map.empty
  where
    fold key count = incrementCounter key count rules

advanceBy :: Int -> Rules -> Counter -> Counter
advanceBy steps rules = last . take (steps + 1) . iterate (advance rules)

incrementCounter :: String -> Int -> Rules -> Counter -> Counter
incrementCounter key count rules counter =
  foldr (`insertAdd` count) counter keys
  where
    keys = fromMaybe [] (Data.Map.lookup key rules)

insertAdd :: (Ord k, Num a) => k -> a -> Map k a -> Map k a
insertAdd key k counter = insert key updatedCount counter
  where
    updatedCount = case Data.Map.lookup key counter of
      Just old -> old + k
      Nothing  -> k

-- Stuff needed to extract results

getFrequencies :: String -> Counter -> Frequencies
getFrequencies template counter =
  insertAdd (head template) 1 frequencies
  where
    frequencies = getFrequencies' counter

getFrequencies' :: Counter -> Frequencies
getFrequencies' = foldrWithKey addUp Data.Map.empty
  where
    addUp key count = insertAdd (key !! 1) count

frequentMinusRare :: Frequencies -> Int
frequentMinusRare frequencies = maximum frequencies' - minimum frequencies'
  where
    frequencies' = elems frequencies

frequentMinusRareAfter :: String -> Int -> Rules -> Counter -> Int
frequentMinusRareAfter polymerTemplate steps rules =
  frequentMinusRare . getFrequencies polymerTemplate . advanceBy steps rules
