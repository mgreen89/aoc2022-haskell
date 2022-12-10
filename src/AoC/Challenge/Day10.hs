module AoC.Challenge.Day10 (
  day10a,
  day10b,
)
where

import AoC.Solution
import Control.DeepSeq (NFData)
import Data.Foldable (foldl')
import Data.List (intercalate)
import GHC.Generics (Generic)
import Text.Read (readMaybe)

import Data.List.Split (chunksOf)
import Debug.Trace

data Instr = Noop | Addx Int deriving (Show, Generic, NFData)

parseInstr :: String -> Either String Instr
parseInstr s = case words s of
  ["noop"] -> Right Noop
  ["addx", n] | Just x <- readMaybe n -> Right $ Addx x
  _ -> Left $ "Invalid instruction: " ++ s

trackX :: [Instr] -> [(Int, Int)]
trackX = reverse . foldl' go [(1, 1)]
 where
  go :: [(Int, Int)] -> Instr -> [(Int, Int)]
  go xs i =
    let (c, x) = head xs
     in case i of
          Noop -> (c + 1, x) : xs
          Addx a -> (c + 2, x + a) : (c + 1, x) : xs

day10a :: Solution [Instr] Int
day10a =
  Solution
    { sParse = traverse parseInstr . lines
    , sShow = show
    , sSolve =
        Right
          . sum
          . fmap (uncurry (*))
          . filter (\(c, x) -> (c - 20) `mod` 40 == 0)
          . trackX
    }

screenWidth :: Int
screenWidth = 40

genDisplay :: [Instr] -> [String]
genDisplay =
  chunksOf screenWidth . reverse . foldl' go [] . trackX
 where
  go :: String -> (Int, Int) -> String
  go a (c, x) =
    let pos = (c - 1) `mod` screenWidth
     in (if abs (pos - x) <= 1 then '#' else '.') : a

day10b :: Solution [Instr] [String]
day10b =
  Solution
    { sParse = traverse parseInstr . lines
    , sShow = \x -> '\n' : intercalate "\n" x
    , sSolve = Right . genDisplay
    }
