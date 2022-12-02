module AoC.Challenge.Day02 (
  day02a,
  day02b,
) where

import AoC.Solution

parseFirst :: Char -> Either String Int
parseFirst = \case
  'A' -> Right 0
  'B' -> Right 1
  'C' -> Right 2
  c -> Left $ "Invalid first: " ++ [c]

parseSecond :: Char -> Either String Int
parseSecond = \case
  'X' -> Right 0
  'Y' -> Right 1
  'Z' -> Right 2
  c -> Left $ "Invalid second: " ++ [c]

parse :: String -> Either String (Int, Int)
parse [a, ' ', b] = do
  a' <- parseFirst a
  b' <- parseSecond b
  pure (a', b')
parse l = Left $ "Invalid input line: " ++ l

day02 :: (Int -> Int -> Int) -> (Int -> Int -> Int) -> Solution [(Int, Int)] Int
day02 shapeScore outcomeScore =
  Solution
    { sParse = traverse parse . lines
    , sShow = show
    , sSolve = Right . sum . fmap (\(a, b) -> shapeScore a b + outcomeScore a b)
    }

shapeScoreA :: Integral a => a -> a -> a
shapeScoreA _ b = b + 1

outcomeScoreA :: Integral a => a -> a -> a
outcomeScoreA a b = ((b - a + 1) `mod` 3) * 3

day02a :: Solution [(Int, Int)] Int
day02a = day02 shapeScoreA outcomeScoreA

shapeScoreB :: Integral a => a -> a -> a
shapeScoreB a b = ((a + b - 1) `mod` 3) + 1

outcomeScoreB :: Integral a => a -> a -> a
outcomeScoreB _ b = b * 3

day02b :: Solution [(Int, Int)] Int
day02b = day02 shapeScoreB outcomeScoreB
