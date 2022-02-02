module Utils.Matrix
  ( Matrix,
    matrixify,
  )
where

import qualified Data.Array.Unboxed as UA
import           Linear.V2

type Matrix a = UA.Array (V2 Int) a

matrixify :: [[a]] -> Matrix a
matrixify vs =
  UA.array
    (V2 0 0, V2 uX uY)
    [(V2 i j, (vs !! j) !! i) | j <- [0 .. uY], i <- [0 .. uX]]
  where
    uX = (length . head $ vs) - 1
    uY = length vs - 1
