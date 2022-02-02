module Problems.Problems
  ( dayToProblemFns,
  )
where

import qualified Data.Map             as M
import           Problems.Day01.Day01
import           Problems.Day02.Day02
import           Problems.Day03.Day03
import           Problems.Day04.Day04
import           Problems.Day05.Day05
import           Problems.Day06.Day06
import           Problems.Day07.Day07
import           Problems.Day08.Day08
import           Problems.Day09.Day09
import           Problems.Day10.Day10
import           Problems.Day11.Day11
import           Problems.Day12.Day12
import           Problems.Day13.Day13
import           Problems.Day14.Day14
import           Problems.Day15.Day15
import           Problems.Day16.Day16
import           Problems.Day17.Day17
import           Problems.Day18.Day18
import           Problems.Day19.Day19
import           Problems.Day20.Day20
import           Problems.Day21.Day21
import           Problems.Day22.Day22
import           Problems.Day23.Day23
import           Problems.Day24.Day24
import           Problems.Day25.Day25

dayToProblemFns :: M.Map Int (IO Int, IO Int)
dayToProblemFns =
  M.fromList
    [ (1, (day01a, day01b)),
      (2, (day02a, day02b)),
      (3, (day03a, day03b)),
      (4, (day04a, day04b)),
      (5, (day05a, day05b)),
      (6, (day06a, day06b)),
      (7, (day07a, day07b)),
      (8, (day08a, day08b)),
      (9, (day09a, day09b)),
      (10, (day10a, day10b)),
      (11, (day11a, day11b)),
      (12, (day12a, day12b)),
      (13, (day13a, day13b)),
      (14, (day14a, day14b)),
      (15, (day15a, day15b)),
      (16, (day16a, day16b)),
      (17, (day17a, day17b)),
      (18, (day18a, day18b)),
      (19, (day19a, day19b)),
      (20, (day20a, day20b)),
      (21, (day21a, day21b)),
      (22, (day22a, day22b)),
      (23, (day23a, day23b)),
      (24, (day24a, day24b)),
      (25, (day25a, day25b))
    ]
