module Problems.Day16.Day16
  ( day16a,
    day16b,
  )
where

import           Data.Char   (digitToInt)
import           Data.Either (fromLeft, fromRight)
import           Data.List   (foldl', splitAt)

day16a = undefined

day16b = undefined

data Packet = Node
  { version  :: Int,
    typeID   :: Int,
    content  :: Either Int [Packet], -- Either Node or Packet?
    bitCount :: Int
  }
  deriving (Show)

versionLength = 3

typeIDLength = 3

headerLength = versionLength + typeIDLength

subPacketBitCount = 15

subPacketCount = 11

main = do
  binary <- hexToBinary <$> readFile "input.txt"
  putStrLn $ "16a: " ++ show ((sum . map versionSum . readPackets) binary)
  putStrLn $ "16b: " ++ show ((evaluateNode . head . readPackets) binary)

-- Binary utils

hexCharToBinary :: Char -> String
hexCharToBinary c =
  case c of
    '\n' -> ""
    '0'  -> "0000"
    '1'  -> "0001"
    '2'  -> "0010"
    '3'  -> "0011"
    '4'  -> "0100"
    '5'  -> "0101"
    '6'  -> "0110"
    '7'  -> "0111"
    '8'  -> "1000"
    '9'  -> "1001"
    'A'  -> "1010"
    'B'  -> "1011"
    'C'  -> "1100"
    'D'  -> "1101"
    'E'  -> "1110"
    'F'  -> "1111"
    _    -> error $ "Bad hex char: " ++ [c]

hexToBinary :: String -> String
hexToBinary = concatMap hexCharToBinary

binaryToInt :: String -> Int
binaryToInt = foldl' f 0
  where
    f acc digit = 2 * acc + digitToInt digit

-- Reading stuff

readVersion :: String -> Int
readVersion = binaryToInt . take versionLength

readTypeID :: String -> Int
readTypeID = binaryToInt . take 3 . drop versionLength

readLengthTypeID :: String -> Int
readLengthTypeID binary = digitToInt $ binary !! 6

readLiteral :: String -> Maybe (Int, Int)
readLiteral hex = do
  literalBinary <- readLiteral' hex
  let bitCount' = 5 * (length literalBinary `div` 4)
  Just (binaryToInt literalBinary, bitCount')

readLiteral' :: String -> Maybe String
readLiteral' binary =
  case leadDigit of
    '0' -> Just literalChunk
    '1' -> case readLiteral' rest of
      Just literalRest -> Just (literalChunk ++ literalRest)
      Nothing          -> Nothing
    _ -> error $ "Bad lead digit: " ++ [leadDigit]
  where
    (quintuple, rest) = splitAt 5 binary
    leadDigit = head quintuple
    literalChunk = tail quintuple

readPackets :: String -> [Packet]
readPackets binary
  | binary == "" = []
  | all (== '0') binary = []
  | otherwise =
    case subPackets of
      Just (content, bitCount') ->
        node : readPackets (drop bitCount' binary)
        where
          node =
            Node
              { version = version,
                typeID = typeID,
                content = content,
                bitCount = bitCount'
              }
      Nothing -> []
  where
    version = readVersion binary
    typeID = readTypeID binary
    lengthTypeID = readLengthTypeID binary

    binary' = if typeID == 4 then drop 6 binary else drop 7 binary
    subPackets = readSubPackets typeID lengthTypeID binary'

readSubPackets :: Int -> Int -> String -> Maybe (Either Int [Packet], Int)
readSubPackets 4 _ binary = do
  (literal, bitCount') <- readLiteral binary
  Just (Left literal, bitCount' + 6)
readSubPackets _ 0 binary = Just (Right content, bitCount' + 15 + 7)
  where
    bitCount' = binaryToInt (take 15 binary)
    binary' = drop 15 binary
    content = readPackets $ take bitCount' binary'
readSubPackets _ 1 binary = Just (Right content, bitCount' + 11 + 7)
  where
    subPacketCount = binaryToInt (take 11 binary)
    binary' = drop 11 binary
    content = take subPacketCount $ readPackets binary'
    bitCount' = sum $ map bitCount content
readSubPackets _ l binary = error $ "malformed binary? " ++ binary

versionSum :: Packet -> Int
versionSum packet =
  case content packet of
    Left _           -> version packet
    Right subPackets -> version packet + sum (map versionSum subPackets)

evaluateNode :: Packet -> Int
evaluateNode packet =
  case typeID packet of
    0 -> (sum . map evaluateNode) subPackets
    1 -> (product . map evaluateNode) subPackets
    2 -> (minimum . map evaluateNode) subPackets
    3 -> (maximum . map evaluateNode) subPackets
    -- Lot of generally unsafe assumptions about packet structure hereon but whatever man
    4 -> fromLeft 0 $ content packet
    5 -> boolToDigit $ evaluateNode (head subPackets) > evaluateNode (subPackets !! 1)
    6 -> boolToDigit $ evaluateNode (head subPackets) < evaluateNode (subPackets !! 1)
    7 -> boolToDigit $ evaluateNode (head subPackets) == evaluateNode (subPackets !! 1)
    _ -> error "Invalid packet Type ID"
  where
    subPackets = fromRight [] $ content packet
    boolToDigit bool = if bool then 1 else 0
