module AoC.Challenge.Day02 (
  day02a,
  day02b,
) where

import AoC.Solution
import Data.Char (ord)

parse :: String -> Either String (Int, Int)
parse [a, _, b] = Right (ord a - ord 'A', ord b - ord 'X')
parse l = Left $ "Invalid input line: " ++ l

day02 :: ((Int, Int) -> Int) -> ((Int, Int) -> Int) -> Solution [(Int, Int)] Int
day02 shapeScore outcomeScore =
  Solution
    { sParse = traverse parse . lines
    , sShow = show
    , sSolve = Right . sum . fmap (\x -> shapeScore x + outcomeScore x)
    }

shapeScoreA :: (Int, Int) -> Int
shapeScoreA (_, b) = b + 1

outcomeScoreA :: (Int, Int) -> Int
outcomeScoreA (a, b) = ((b - a + 1) `mod` 3) * 3

day02a :: Solution [(Int, Int)] Int
day02a = day02 shapeScoreA outcomeScoreA

shapeScoreB :: (Int, Int) -> Int
shapeScoreB (a, b) = ((a + b - 1) `mod` 3) + 1

outcomeScoreB :: (Int, Int) -> Int
outcomeScoreB (_, b) = b * 3

day02b :: Solution [(Int, Int)] Int
day02b = day02 shapeScoreB outcomeScoreB
