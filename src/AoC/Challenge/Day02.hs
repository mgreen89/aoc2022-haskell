module AoC.Challenge.Day02 (
  day02a,
  day02b,
) where

import AoC.Solution
import Data.Char (ord)

parse :: [Char] -> Either String (Int, Int)
parse [a, _, b] = Right (ord a - ord 'A', ord b - ord 'X')
parse _ = Left "Invalid input"

shapeScoreA :: (Int, Int) -> Int
shapeScoreA (_, b) = b + 1

outcomeScoreA :: (Int, Int) -> Int
outcomeScoreA (a, b) =
  ((b - a + 1) `mod` 3) * 3

day02a :: Solution [(Int, Int)] Int
day02a =
  Solution
    { sParse = traverse parse . lines
    , sShow = show
    , sSolve = Right . sum . fmap (\x -> shapeScoreA x + outcomeScoreA x)
    }

shapeScoreB :: (Int, Int) -> Int
shapeScoreB (a, b) = ((a + b - 1) `mod` 3) + 1

outcomeScoreB :: (Int, Int) -> Int
outcomeScoreB (a, b) = b * 3

day02b :: Solution [(Int, Int)] Int
day02b =
  Solution
    { sParse = traverse parse . lines
    , sShow = show
    , sSolve = Right . sum . fmap (\x -> shapeScoreB x + outcomeScoreB x)
    }
