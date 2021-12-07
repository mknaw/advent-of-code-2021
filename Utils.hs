module Utils
    ( splitChar,
    ) where

splitChar :: Char -> String -> [String]
splitChar c s =
  case dropWhile (== c) s of
    "" -> []
    s' -> w : splitChar c s''
      where
        (w, s'') = break (== c) s'

