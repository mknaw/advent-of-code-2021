
main = do
  contents <- readFile "input.txt"
  let points = map (\x -> read x :: Integer) (lines contents)
  -- Read a list of integers and count increases on a rolling basis
  putStrLn $ "1a: " ++ show (countIncreases points)
  -- Now count increases on a rolling windowed sum
  putStrLn $ "1b: " ++ show (countWindowed points)

countIncreases :: (Ord a) => [a] -> Integer
countIncreases [_]    = 0
countIncreases (x:xs) =
  if x < head xs
  then 1 + countIncreases xs
  else countIncreases xs
countIncreases _      = 0

countWindowed :: (Num a, Ord a) => [a] -> Integer
countWindowed (a:b:c:d:es) =
  if a + b + c < b + c + d
  then 1 + countWindowed (b:c:d:es)
  else countWindowed (b:c:d:es)
countWindowed _            = 0
