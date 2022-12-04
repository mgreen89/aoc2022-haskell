{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day04 (
  day04a,
  day04b
) where

import AoC.Solution
import Control.Monad ((<=<))
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

listTup2 :: [a] -> Maybe (a, a)
listTup2 [x, y] = Just (x, y)
listTup2 _ = Nothing

parseLine :: String -> Maybe ((Int, Int), (Int, Int))
parseLine l = do
  (f, s) <- listTup2 . splitOn "," $ l
  (a, b) <- listTup2 <=< traverse readMaybe . splitOn "-" $ f
  (c, d) <- listTup2 <=< traverse readMaybe . splitOn "-" $ s
  pure ((a, b), (c, d))

checkA :: ((Int, Int), (Int, Int)) -> Bool
checkA ((a, b), (c, d)) =
  a <= c && b >= d || a >= c && b <= d

run :: (((Int, Int), (Int, Int)) -> Bool) -> [String] -> Either String Int
run check ls = case traverse parseLine ls of
  Just l -> Right . length . filter id . fmap check $ l
  Nothing -> Left "Invalid line"

day04a :: Solution [String] Int
day04a =
  Solution
    { sParse = Right . lines
    , sShow = show
    , sSolve = run checkA
    }

checkB :: ((Int, Int), (Int, Int)) -> Bool
checkB ((a, b), (c, d)) =
  a <= c && b >= c || a <= d && b >= d || c <= a && d >= a || c <= b && d >= b

day04b :: Solution [String] Int
day04b =
  Solution
    { sParse = Right . lines
    , sShow = show
    , sSolve = run checkB
    }
