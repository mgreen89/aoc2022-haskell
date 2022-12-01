module AoC.Challenge.Day01 (
  day01a,
  day01b,
) where

import AoC.Solution
import Data.List (sortBy)
import Data.List.Split (splitOn)
import Text.Read (readEither)

parse :: String -> Either String [[Int]]
parse = traverse (traverse readEither . lines) . splitOn "\n\n"

day01a :: Solution [[Int]] Int
day01a =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve = Right . maximum . fmap sum
    }

day01b :: Solution [[Int]] Int
day01b =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve = Right . sum . take 3 . sortBy (flip compare) . fmap sum
    }
