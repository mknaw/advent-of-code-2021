import           Data.List (scanl')
import           Utils     (splitChar)

main = do
    contents <- readFile "input.txt"
    let positions = (\k -> read k :: Int) <$> splitChar ',' contents
    let minPos = minimum positions
    let maxPos = maximum positions
    -- There are probably smarter ways than brute force over the whole range...
    putStrLn $ "7a: " ++ show (minimum $ map (`fuelSpent` positions) [minPos..maxPos])
    let triangles = map (\k -> (k + 1) * k `div` 2) [0..(maxPos - minPos)]
    let triangleDistances =
          map (swapTriangles triangles . distancesFromEnd positions) [minPos..maxPos]
    putStrLn $ "7b: " ++ show (minimum $ map sum triangleDistances)

distancesFromEnd :: [Int] -> Int -> [Int]
distancesFromEnd positions end = map (\pos -> abs $ pos - end) positions

fuelSpent :: Int -> [Int] -> Int
fuelSpent end = sum . map (\pos -> abs $ pos - end)

swapTriangles triangles = map (triangles !!)
