import Data.List (foldl', sort)

type Stack a = [a]

data LineStatus = OK
                | Corrupted Char
                | Incomplete String
                  deriving (Eq, Show)

isCorrupted :: LineStatus -> Bool
isCorrupted status =
    case status of
      Corrupted _ -> True
      _           -> False

isIncomplete :: LineStatus -> Bool
isIncomplete status =
    case status of
      Incomplete _ -> True
      _            -> False

main = do
    chunks <- fmap lines (readFile "input.txt")
    let tested = map testLine chunks
    let corrupted = filter isCorrupted tested
    putStrLn $ "10a: " ++ show (sum $ map score corrupted)
    let incomplete = filter isIncomplete tested
    let incompleteScores = sort $ map score incomplete
    putStrLn $ "10b: " ++ show (pickMiddle incompleteScores)

push :: Stack a -> a -> Stack a
push stack el = el : stack

isOpeningChar :: Char -> Bool
isOpeningChar c = c `elem` ['[', '{', '(', '<']

getClosingChar :: Char -> Char
getClosingChar c
    | c == '[' = ']'
    | c == '{' = '}'
    | c == '(' = ')'
    | c == '<' = '>'
    | otherwise = error "uhoh"

isCharPair :: Char -> Char -> Bool
isCharPair open close = getClosingChar open == close

testLine :: String -> LineStatus
testLine = testLine' []

testLine' :: Stack Char -> String -> LineStatus
testLine' stack "" = if null stack then OK else Incomplete (complete stack)
testLine' stack (c:rest)
    | isOpeningChar c           = testLine' (push stack c) rest
    | isCharPair (head stack) c = testLine' (tail stack) rest
    | otherwise                 = Corrupted c

complete :: Stack Char -> String
complete = map getClosingChar

score :: LineStatus -> Int
score status =
    case status of
      Corrupted c -> corruptedCharPoints c
      Incomplete rest -> scoreIncomplete $ map incompleteCharPoints rest
      _ -> 0
  where
    corruptedCharPoints c
      | c == ')' = 3
      | c == ']' = 57
      | c == '}' = 1197
      | c == '>' = 25137
      | otherwise = error "uhoh"
    incompleteCharPoints c
      | c == ')' = 1
      | c == ']' = 2
      | c == '}' = 3
      | c == '>' = 4
      | otherwise = error "uhoh"
    scoreIncomplete :: [Int] -> Int
    scoreIncomplete = foldl' (\a b -> 5 * a + b) 0

pickMiddle :: [a] -> a
pickMiddle lst = lst !! k
  where
    k = length lst `div` 2
