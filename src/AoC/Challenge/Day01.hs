module AoC.Challenge.Day01 (
  day01a,
  day01b,
) where

import AoC.Solution
import Data.List (sortBy)
import Data.List.Split (splitOn)

day01a :: Solution [[Int]] Int
day01a =
  Solution
    { sParse = Right . fmap (fmap read . lines) . splitOn "\n\n"
    , sShow = show
    , sSolve = Right . maximum . fmap sum
    }

day01b :: Solution [[Int]] Int
day01b =
  Solution
    { sParse = Right . fmap (fmap read . lines) . splitOn "\n\n"
    , sShow = show
    , sSolve = Right . sum . take 3 . sortBy (flip compare) . fmap sum
    }
