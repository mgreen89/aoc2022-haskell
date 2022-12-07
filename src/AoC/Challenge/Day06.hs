module AoC.Challenge.Day06 (
  day06a,
  day06b,
)
where

import AoC.Solution
import AoC.Util (maybeToEither)
import Control.Monad (foldM)
import Data.List (tails)
import qualified Data.Set as S
import Text.Printf (printf)

allDifferent :: (Ord a) => [a] -> Bool
allDifferent x = length x == S.size (S.fromList x)

windows :: Int -> [a] -> [[a]]
windows n = foldr (zipWith (:)) (repeat []) . take n . tails

solve :: Int -> String -> Either String Int
solve n s =
  case foldM check n (windows n s) of
    Right c -> Left $ printf "Could not find %d different" n
    Left i -> Right i
 where
  check :: Int -> String -> Either Int Int
  check c s = if allDifferent s then Left c else Right (c + 1)

day06a :: Solution String Int
day06a = Solution{sParse = Right, sShow = show, sSolve = solve 4}

day06b :: Solution String Int
day06b = Solution{sParse = Right, sShow = show, sSolve = solve 14}
