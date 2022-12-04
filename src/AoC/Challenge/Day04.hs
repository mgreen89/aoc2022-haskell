module AoC.Challenge.Day04 (
  day04a,
  day04b,
) where

import AoC.Solution
import AoC.Util (maybeToEither)
import Control.Monad ((<=<))
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

listTup2 :: [a] -> Maybe (a, a)
listTup2 [x, y] = Just (x, y)
listTup2 _ = Nothing

type Areas = ((Int, Int), (Int, Int))

parseLine :: String -> Maybe Areas
parseLine =
  listTup2
    <=< traverse (listTup2 <=< traverse readMaybe . splitOn "-")
      . splitOn ","

day04 :: (Areas -> Bool) -> Solution [Areas] Int
day04 check =
  Solution
    { sParse = maybeToEither "Invalid input" . traverse parseLine . lines
    , sShow = show
    , sSolve = Right . length . filter id . fmap check
    }

checkA :: Areas -> Bool
checkA ((a, b), (c, d)) =
  a <= c && b >= d || a >= c && b <= d

day04a :: Solution [Areas] Int
day04a = day04 checkA

checkB :: Areas -> Bool
checkB ((a, b), (c, d)) =
  not (b < c || a > d)

day04b :: Solution [Areas] Int
day04b = day04 checkB
