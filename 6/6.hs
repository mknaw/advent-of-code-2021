import           Data.List (iterate)
import           Utils     (splitChar)

main = do
    contents <- readFile "input.txt"
    let integers = (\k -> read k :: Int) <$> splitChar ',' contents
    let initialCounts = map (\x -> length $ filter (== x) integers) [0..8]
    putStrLn $ "6a: " ++ show (getFinalCount 80 initialCounts)
    putStrLn $ "6b: " ++ show (getFinalCount 256 initialCounts)

advance :: [Int] -> [Int]
advance (zero:one:two:three:four:five:six:seven:[eight]) =
    one:two:three:four:five:six:(seven + zero):eight:[zero]
advance _ = error "uhoh"

getFinalCount :: Int -> [Int] -> Int
getFinalCount days = sum . last . take (days + 1) . iterate advance
