module AoC.Challenge.Day06 (
  day06a,
  day06b
)
where

import AoC.Solution
import AoC.Util (maybeToEither)
import Control.Applicative (getZipList, ZipList(..))
import Data.List (zip4)

solve :: Int -> String -> Either String Int
solve n s =
  let f = foldr check (Right n) . reverse . getZipList . traverse ZipList . take n . iterate (drop 1) $ s
  in case f of
    Right c -> Left "Could not find 4 different"
    Left i -> Right i
 where
  check :: String -> Either Int Int -> Either Int Int
  check _ (Left i) = Left i
  check s (Right c) = if allDifferent s then Left c else Right (c + 1)

  allDifferent :: (Eq a) => [a] -> Bool
  allDifferent = \case
    [] -> True
    (x : xs) -> x `notElem` xs && allDifferent xs

day06a :: Solution String Int
day06a = Solution{sParse = Right, sShow = show, sSolve = solve 4}

day06b :: Solution String Int
day06b = Solution{sParse = Right, sShow = show, sSolve = solve 14}
