import Data.List (foldl')

main = do
  contents <- readFile "input.txt"
  let vectors = map readLine (lines contents)
  -- Sum the distances in the input file by direction and multiply
  let vectorSum = sumVectors vectors
  putStrLn $ "2a: " ++ show (uncurry (*) vectorSum)
  -- Now the summation also has something like an angled vector approach
  let aimedVectorSum = sumAimedVectors vectors
  putStrLn $ "2b: " ++ show (uncurry (*) aimedVectorSum)

readLine :: String -> (String, Integer)
readLine line = (direction, distance)
  where line' = words line
        direction = head line'
        distance = read (line'!!1) :: Integer

sumVectors :: [(String, Integer)] -> (Integer, Integer)
sumVectors = foldr f (0, 0)
  where f (direction, distance) (horizontal, depth)
          | direction == "forward" = (horizontal + distance, depth)
          | direction == "up"      = (horizontal, depth - distance)
          | direction == "down"    = (horizontal, depth + distance)
          | otherwise              = error "unexpected input"

sumAimedVectors :: [(String, Integer)] -> (Integer, Integer)
sumAimedVectors vectors = sumAimedVectors' vectors 0 (0, 0)

sumAimedVectors' :: [(String, Integer)] -> Integer -> (Integer, Integer) -> (Integer, Integer)
sumAimedVectors' [] _ vectorSum = vectorSum
sumAimedVectors' vectors aim (horizontal, depth) =
    sumAimedVectors' (tail vectors) aim' vectorSum'
  where (direction, distance) = head vectors
        aim'
          | direction == "down" = aim + distance
          | direction == "up"   = aim - distance
          | otherwise           = aim
        vectorSum'
          | direction == "forward" = (horizontal + distance, depth + aim * distance)
          | otherwise              = (horizontal, depth)
