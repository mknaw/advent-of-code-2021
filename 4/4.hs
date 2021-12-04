import           Data.List (transpose)

data Cell = Cell
    { number :: Integer
    , marked :: Bool
    }

initCell :: Integer -> Cell
initCell k = Cell { number=k, marked=False }

-- Pretty print for debugging purposes
instance Show Cell where
  show c =
      pad k'
    where
      k' =
        if marked c
          then show (number c) ++ "*"
          else show (number c) ++ " "
      pad s
        | length s < 3 = pad (s ++ " ")
        | otherwise    = s

newtype Row = Row [Cell]

instance Show Row where
  show (Row cells) = unwords $ map show cells

getCells :: Row -> [Cell]
getCells (Row cells) = cells

newtype Board = Board [Row]

instance Show Board where
  show (Board rows) = unlines $ map show rows

getMarkedCells :: Board -> [[Bool]]
getMarkedCells (Board rows) = map (map marked) $ getCells <$> rows

main = do
  contents <- readFile "input.txt"
  -- Read the bingo boards
  let (called':boards') = lines contents
  let called = getIntegers ',' called'
  let boards = readBoards boards'
  -- Part a - ID first winning board
  let (lastCalled, winningBoard) = firstWinner called boards
  let sumUnmarked = getSumUnmarked winningBoard
  putStrLn $ "4a: " ++ show (sumUnmarked * lastCalled)
  -- Part b - ID last winning board
  let (lastCalled, winningBoard) = lastWinner called boards
  let sumUnmarked = getSumUnmarked winningBoard
  putStrLn $ "4b: " ++ show (sumUnmarked * lastCalled)

splitChar :: Char -> String -> [String]
splitChar c s =
  case dropWhile (== c) s of
    "" -> []
    s' -> w : splitChar c s''
      where
        (w, s'') = break (== c) s'

getIntegers :: Char -> String -> [Integer]
getIntegers sep = map (\x -> read x :: Integer) . splitChar sep

readBoards :: [String] -> [Board]
readBoards contents =
  case dropWhile null contents of
    [] -> []
    xs -> board : readBoards rest
      where
        (board', rest) = break null xs
        boardNumbers = map (getIntegers ' ') board'
        board = Board $ map (Row <$> map initCell) boardNumbers

applyNumberCall :: Integer -> Board -> Board
applyNumberCall x (Board rows) =
    Board $ map (Row . map newCell) (getCells <$> rows)
  where
    newCell Cell { number=k', marked=previous } =
      if k' == x
        then Cell { number=k', marked=True }
        else Cell { number=k', marked=previous }

firstWinner :: [Integer] -> [Board] -> (Integer, Board)
firstWinner [] _ = error "uhoh"
firstWinner (call:rest) boards
    | not $ null winningBoards = (call, head winningBoards)
    | otherwise                = firstWinner rest boards'
  where
    boards' = applyNumberCall call <$> boards
    winningBoards = filter isWinningBoard boards'

lastWinner :: [Integer] -> [Board] -> (Integer, Board)
lastWinner [] _ = error "uhoh"
lastWinner (call:rest) boards
    | haveLastWinner = (call, head boards')
    | otherwise       = lastWinner rest (filter (not . isWinningBoard) boards')
  where
    boards' = applyNumberCall call <$> boards
    haveLastWinner = length boards' == 1 && isWinningBoard (head boards')

isWinningBoard :: Board -> Bool
isWinningBoard board
    | applyCheck (any and) boolRows = True
    -- Misread the instructions... diagonals don't count. But, if they did:
    -- applyCheck (and . getDiagonal) boolRows = True
    | otherwise = False
  where
    boolRows = getMarkedCells board
    rotate = transpose . map reverse
    applyCheck check board' = check board' || check (rotate board')

-- getDiagonal :: [[a]] -> [a]
-- getDiagonal []           = []
-- getDiagonal (first:rest) = head first : getDiagonal (map tail rest)

getSumUnmarked :: Board -> Integer
getSumUnmarked (Board rows) =
    sum $ number <$> filtered
  where
    flat = concat $ getCells <$> rows
    filtered = filter (not . marked) flat
