module AoC.Challenge.Day25 (
  day25a,
)
where

import AoC.Solution
import AoC.Util (maybeToEither)
import Control.Applicative (liftA2)
import Data.List (unfoldr)

snafuToInt :: String -> Maybe Int
snafuToInt =
  foldl (\a c -> liftA2 (+) (charToInt c) (fmap (* 5) a)) (Just 0)
 where
  charToInt = \case
    '=' -> Just (-2)
    '-' -> Just (-1)
    '0' -> Just 0
    '1' -> Just 1
    '2' -> Just 2
    _ -> Nothing

intToSnafu :: Int -> String
intToSnafu =
  reverse . unfoldr go
 where
  go :: Int -> Maybe (Char, Int)
  go 0 = Nothing
  go i =
    Just $ case i `mod` 5 of
      0 -> ('0', i `div` 5)
      1 -> ('1', i `div` 5)
      2 -> ('2', i `div` 5)
      3 -> ('=', (i `div` 5) + 1)
      4 -> ('-', (i `div` 5) + 1)
      _ -> error "Mod function"

day25a :: Solution [String] String
day25a =
  Solution
    { sParse = Right . lines
    , sShow = id
    , sSolve =
        fmap (intToSnafu . sum)
          . maybeToEither "Invalid snafu number"
          . traverse snafuToInt
    }
