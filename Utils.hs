module Utils
    ( splitChar
    , splitString
    ) where

import           Data.List (findIndex, isPrefixOf, tails)

splitChar :: Char -> String -> [String]
splitChar c s =
  case dropWhile (== c) s of
    "" -> []
    s' -> w : splitChar c s''
      where
        (w, s'') = break (== c) s'

-- Pilfered from stackoverflow
findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)

splitString :: String -> String -> [String]
splitString sp s
    | sp `isPrefixOf` s = splitString sp . drop (length sp) $ s
    | s == "" = []
    | otherwise =
        case findString sp s of
          Just idx -> take idx s : splitString sp (drop (idx + length sp) s)
          Nothing  -> [s]
