module AoC.Challenge.Day06 (
  day06a,
  day06b
)
where

import AoC.Solution
import AoC.Util (maybeToEither)
import Control.Applicative (getZipList, ZipList(..))
import Control.Monad (foldM)
import Data.List (tails)

windows :: Int -> [a] -> [[a]]
windows n = getZipList . traverse ZipList . take n . tails

solve :: Int -> String -> Either String Int
solve n s =
  let f = foldM check n $ windows n s
  in case f of
    Right c -> Left "Could not find 4 different"
    Left i -> Right i
 where
  check :: Int -> String -> Either Int Int
  check c s = if allDifferent s then Left c else Right (c + 1)

  allDifferent :: (Eq a) => [a] -> Bool
  allDifferent = \case
    [] -> True
    (x : xs) -> x `notElem` xs && allDifferent xs

day06a :: Solution String Int
day06a = Solution{sParse = Right, sShow = show, sSolve = solve 4}

day06b :: Solution String Int
day06b = Solution{sParse = Right, sShow = show, sSolve = solve 14}
