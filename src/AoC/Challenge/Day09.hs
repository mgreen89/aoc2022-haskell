{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day09 (
  day09a,
)
where

-- , day09b

import AoC.Solution
import Control.DeepSeq (NFData)
import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Linear (V2 (..))
import Text.Read (readEither)

type Point = V2 Int

data Dir = R | U | L | D deriving (Show, Generic, NFData)

data Move = Move {d :: Dir, l :: Int} deriving (Show, Generic, NFData)

parseMove :: String -> Either String Move
parseMove s = case words s of
  ["R", n] -> Move R <$> readEither n
  ["U", n] -> Move U <$> readEither n
  ["L", n] -> Move L <$> readEither n
  ["D", n] -> Move D <$> readEither n
  _ -> Left $ "Invalid line: " ++ s

dir :: Dir -> Point
dir = \case
  R -> V2 0 1
  U -> V2 (-1) 0
  L -> V2 0 (-1)
  D -> V2 1 0

move :: Point -> Move -> [Point]
move p m =
  take m.l . drop 1 . iterate (+ dir m.d) $ p

moveRope :: (Point, Point) -> Move -> [(Point, Point)]
moveRope (h, t) m =
  foldl' go [(h, t)] (move h m)
 where
  go :: [(Point, Point)] -> Point -> [(Point, Point)]
  go l h =
    let t = snd $ head l
     in if t `elem` close h then (h, t) : l else (h, h - dir m.d) : l

  close :: Point -> [Point]
  close p = [p + d | d <- V2 <$> [-1, 0, 1] <*> [-1, 0, 1]]

solveA :: [Move] -> Int
solveA =
  S.size . snd . foldl' go ((V2 0 0, V2 0 0), S.empty)
 where
  go :: ((Point, Point), Set Point) -> Move -> ((Point, Point), Set Point)
  go ((h, t), s) m =
    let moves = moveRope (h, t) m
     in (head moves, S.union (S.fromList (snd <$> moves)) s)

day09a :: Solution [Move] Int
day09a =
  Solution
    { sParse = traverse parseMove . lines
    , sShow = show
    , sSolve = Right . solveA
    }

day09b :: Solution _ _
day09b = Solution{sParse = Right, sShow = show, sSolve = Right}
