module AoC.Challenge.Day07 (
  day07a,
  day07b,
) where

import AoC.Solution
import AoC.Util (maybeToEither)
import Control.Applicative (asum)
import Data.Foldable (foldl')
import Data.List (sort, tails)
import Data.Map (Map)
import qualified Data.Map as M
import Text.Read (readMaybe)

getSizes :: [String] -> Map [String] Int
getSizes = snd . foldl' go ([], M.empty)
 where
  go :: ([String], Map [String] Int) -> String -> ([String], Map [String] Int)
  go (pwd, m) l = case words l of
    ["$", "cd", "/"] -> ([], m)
    ["$", "cd", ".."] -> (tail pwd, m)
    ["$", "cd", d] -> (d : pwd, m)
    ["$", _] -> (pwd, m)
    ["dir", _] -> (pwd, m)
    [n, _] | Just size <- readMaybe n -> handleFile size (pwd, m)
    _ -> error "Invalid line!"

  handleFile :: Int -> ([String], Map [String] Int) -> ([String], Map [String] Int)
  handleFile size (pwd, m) = (pwd, M.unionWith (+) m $ M.fromList (fmap (,size) (tails pwd)))

day07a :: Solution [String] Int
day07a =
  Solution
    { sParse = Right . lines
    , sShow = show
    , sSolve = Right . sum . M.filter (<= 100000) . getSizes
    }

solveB :: Map [String] Int -> Either String Int
solveB sizes =
  maybeToEither "No dir large enough" . asum . fmap go . sort $ M.elems sizes
 where
  free = 70000000 - sizes M.! []
  reqSize = 30000000 - free
  go :: Int -> Maybe Int
  go i
    | i >= reqSize = Just i
    | otherwise = Nothing

day07b :: Solution [String] Int
day07b =
  Solution
    { sParse = Right . lines
    , sShow = show
    , sSolve = solveB . getSizes
    }
