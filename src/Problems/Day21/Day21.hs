module Problems.Day21.Day21
  ( day21a,
    day21b,
  )
where

import           Control.Monad.Trans.State.Lazy
import           Data.Bifunctor                 as B
import           Data.Char                      (isDigit)
import           Data.Function                  (on)
import qualified Data.List                      as L
import           Data.Map                       ((!))
import qualified Data.Map                       as M
import           Data.Maybe                     (mapMaybe)

day21a = undefined

day21b = undefined

-- This one is pretty sloppy...
-- need to tap in to functional galaxy brain + refactor

type Die = Int

type Position = Int

type Score = Int

data Player = Player
  { _position :: Position,
    _score    :: Score
  }
  deriving (Show)

type Players = (Player, Player)

data Game = Game
  { _turns   :: Int,
    _die     :: Int,
    _players :: M.Map Int Player,
    _queue   :: [Int]
  }
  deriving (Show)

data SequenceTracker = SequenceTracker
  { _pos      :: Int,
    _total    :: Int,
    _sequence :: [Int]
  }
  deriving (Show)

main = do
  players <- map initPlayer . lines <$> readFile "input.txt"
  let [initialA, initialB] = map _position players
  let game = initGame players
  let (_, Game {_players = players, _turns = turns}) = runState playGame game
  let (_, loser : _) = L.partition hasWon (M.elems players)
  putStrLn $ "21a: " ++ show (_score loser * 3 * turns)
  let turnsA = makeTurnsToUniverses $ viableSequences initialA
  let turnsB = makeTurnsToUniverses $ viableSequences initialB
  let aWinCount = getWins True turnsA turnsB
  let bWinCount = getWins False turnsB turnsA
  putStrLn $ "21b: " ++ show (max aWinCount bWinCount)

-- Parse input

initPlayer :: String -> Player
initPlayer s = Player {_position = position, _score = 0}
  where
    position = read (filter isDigit . dropWhile (/= ':') $ s) :: Int

initGame :: [Player] -> Game
initGame players =
  Game
    { _players = players',
      _queue = L.cycle $ M.keys players',
      _die = 0,
      _turns = 0
    }
  where
    players' = M.fromList $ zip [1 ..] players

-- Play game

playGame :: State Game Game
playGame = do
  takeTurn
  game <- get
  if isOver game
    then return game
    else playGame

roll :: State Game Int
roll = do
  die <- increment . _die <$> get
  game <- get
  put $ game {_die = die}
  return die
  where
    increment d
      | d == 100 = 1
      | otherwise = d + 1

takeTurn :: State Game ()
takeTurn = do
  -- TODO would like a nicer way to do this
  die <- roll
  die' <- roll
  die'' <- roll

  game <- get
  let queue = _queue game
  let playerID = head queue
  let players = _players game
  let player = players ! playerID
  let position = advance (_position player) (die + die' + die'')
  let score = _score player + position
  let player' = player {_position = position, _score = score}
  put $
    game
      { _turns = _turns game + 1,
        _players = M.update (const (Just player')) playerID players,
        _queue = tail queue
      }

advance :: Int -> Int -> Int
advance position by = ((position - 1 + by) `rem` 10) + 1

hasWon :: Player -> Bool
hasWon player = _score player >= 1000

isOver :: Game -> Bool
isOver = any hasWon . M.elems . _players

numberToDiceCombos :: M.Map Int Int
numberToDiceCombos =
  M.fromList
    [ (3, 1),
      (4, 3),
      (5, 6),
      (6, 7),
      (7, 6),
      (8, 3),
      (9, 1)
    ]

getNexts :: Int -> [(Int, Int)]
getNexts k = zip [3 .. 9] nextPositions
  where
    nextPositions = take 7 . drop (k + 2) . cycle $ [1 .. 10]

viableSequences :: Int -> [SequenceTracker]
viableSequences startingPosition =
  viableSequences' (SequenceTracker startingPosition 0 [])

addToSequence :: SequenceTracker -> (Int, Int) -> SequenceTracker
addToSequence (SequenceTracker pos total sequence) (roll, position) =
  SequenceTracker
    { _pos = position,
      _total = total + position,
      -- TODO probably better to insert at head
      _sequence = sequence ++ [roll]
    }

viableSequences' :: SequenceTracker -> [SequenceTracker]
viableSequences' tracker@(SequenceTracker position total _)
  | total >= 21 = [tracker]
  | otherwise = concatMap viableSequences' nexts
  where
    isValid :: SequenceTracker -> Bool
    isValid SequenceTracker {_total = total} = total <= 21

    nexts = map (addToSequence tracker) $ getNexts position

diceCombosPerTurn :: Int
diceCombosPerTurn = sum . M.elems $ numberToDiceCombos

waysToRollSequence :: [Int] -> Int
waysToRollSequence = product . mapMaybe (`M.lookup` numberToDiceCombos)

makeTurnsToUniverses :: [SequenceTracker] -> M.Map Int Int
makeTurnsToUniverses = makeTurnsToUniverses' M.empty

makeTurnsToUniverses' :: M.Map Int Int -> [SequenceTracker] -> M.Map Int Int
makeTurnsToUniverses' map' [] = map'
makeTurnsToUniverses' map' (tracker : rest) =
  makeTurnsToUniverses' updatedMap rest
  where
    waysToRoll = waysToRollSequence (_sequence tracker)
    rollCount = length $ _sequence tracker
    updatedMap = M.insertWith (+) rollCount waysToRoll map'

getWins :: Bool -> M.Map Int Int -> M.Map Int Int -> Int
getWins tieWins a b = sum $ zipWith (*) wonBy notWonBy
  where
    (minTurn, _) = M.findMin a
    a' = replicate (minTurn - 1) 0 ++ M.elems a
    wonBy = if tieWins then drop 1 a' else a'
    notWonBy = M.elems $ getNotWonBy b

getNotWonBy :: M.Map Int Int -> M.Map Int Int
getNotWonBy turns = M.unionWith (-) all wonBy
  where
    wonBy = M.fromList $ zip (M.keys turns) $ L.scanl1 (\a b -> 27 * a + b) (M.elems turns)
    (maxTurn, _) = M.findMax turns
    all = M.fromList $ zip [1 .. maxTurn] $ map (27 ^) [1 .. maxTurn]
