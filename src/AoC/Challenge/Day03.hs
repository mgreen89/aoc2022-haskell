module AoC.Challenge.Day03 (
  day03a,
  day03b,
) where

import AoC.Solution
import Data.Char (isLower, ord)
import qualified Data.IntSet as IS
import Data.List (foldl1')
import Data.List.Split (chunksOf)

prio :: Char -> Int
prio c
  | isLower c = ord c - ord 'a' + 1
  | otherwise = ord c - ord 'A' + 27

getCommon :: [String] -> Int
getCommon = IS.findMin . foldl1' IS.intersection . fmap (IS.fromList . fmap prio)

day03 :: ([String] -> [[String]]) -> Solution [String] Int
day03 prep =
  Solution
    { sParse = Right . lines
    , sShow = show
    , sSolve = Right . sum . fmap getCommon . prep
    }

day03a :: Solution [String] Int
day03a = day03 (fmap (\x -> chunksOf (length x `div` 2) x))

day03b :: Solution [String] Int
day03b = day03 (chunksOf 3)
