import Prelude hiding (traverse)
import Data.Char      (isLower)
import Data.List      (group, sort)
import Data.Maybe     (mapMaybe)
import Utils          (splitChar)

type Node = String
type Edge = (Node, Node)
type Path = [Node]

main = do
    contents <- lines <$> readFile "input.txt"
    let graph = map (pairify . splitChar '-') contents
    putStrLn $ "12a: " ++ show (length (traverse graph criteriumA ["start"]))
    putStrLn $ "12b: " ++ show (length (traverse graph criteriumB ["start"]))

pairify :: [a] -> (a, a)
pairify [a, b] = (a, b)
pairify _      = error "uhoh"

getNexts :: Node -> [Edge] -> [Node]
getNexts current = mapMaybe getNext
  where
    -- TODO bet there's some nice ~functional~ way to do this check
    getNext edge
        | fst edge == current = Just (snd edge)
        | snd edge == current = Just (fst edge)
        | otherwise = Nothing

isSmall :: Node -> Bool
isSmall = all isLower

criteriumA :: Path -> Node -> Bool
criteriumA path node = node `notElem` smallVisited
  where
    smallVisited = filter isSmall path

criteriumB :: Path -> Node -> Bool
criteriumB path node
    | node == "start" = False
    | not $ isSmall node = True
    | highestFrequency == 1 = True
    | node `notElem` smallVisited = True
    | otherwise = False
  where
    smallVisited = filter isSmall path
    highestFrequency = (maximum . map length . group . sort) smallVisited
    occurrences = length (filter (== node) path)

traverse :: [Edge] -> (Path -> Node -> Bool) -> Path -> [Path]
traverse graph criteriumFn path =
    if current == "end"
      then [path]
      else concatMap (traverse graph criteriumFn) paths
  where
    current = last path
    nexts = filter (criteriumFn path) $ getNexts current graph
    paths = map (\next -> path ++ [next]) nexts
