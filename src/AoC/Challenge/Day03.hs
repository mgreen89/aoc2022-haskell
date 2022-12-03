module AoC.Challenge.Day03 (
  day03a,
  day03b,
) where

import AoC.Solution
import Data.Char (isLower, ord)
import Data.IntSet (findMin, fromList, intersection)
import Data.List (foldl1', splitAt)
import Data.List.Split (chunksOf)

prio :: Char -> Int
prio c
  | isLower c = ord c - ord 'a' + 1
  | otherwise = ord c - ord 'A' + 27

getCommonPrio :: String -> Int
getCommonPrio inp =
  let (a, b) = splitAt (length inp `div` 2) inp
      (a', b') = (fromList $ prio <$> a, fromList $ prio <$> b)
   in findMin $ intersection a' b'

day03a :: Solution [String] Int
day03a =
  Solution
    { sParse = Right . lines
    , sShow = show
    , sSolve = Right . sum . fmap getCommonPrio
    }

getCommon :: [String] -> Int
getCommon = findMin . foldl1' intersection . fmap (fromList . fmap prio)

day03b :: Solution [String] Int
day03b =
  Solution
    { sParse = Right . lines
    , sShow = show
    , sSolve = Right . sum . fmap getCommon . chunksOf 3
    }
