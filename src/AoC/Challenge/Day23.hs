{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day23 (
  day23a,
  day23b,
)
where

import AoC.Common.Point (Dir (..), allNeighbs, boundingBox', dirPoint, dirRot)
import AoC.Solution
import Control.DeepSeq (NFData)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Linear (V2 (..))

type Point = V2 Int

parseMap :: String -> Set Point
parseMap =
  S.fromList
    . mapMaybe (\(p, c) -> if c == '#' then Just p else Nothing)
    . concat
    . zipWith (\y -> zipWith (\x -> (V2 x y,)) [0 ..]) [0 ..]
    . lines

checks :: Dir -> [Point]
checks U = [V2 (-1) (-1), V2 0 (-1), V2 1 (-1)]
checks R = [V2 1 (-1), V2 1 0, V2 1 1]
checks D = [V2 (-1) 1, V2 0 1, V2 1 1]
checks L = [V2 (-1) (-1), V2 (-1) 0, V2 (-1) 1]

propose :: Dir -> Set Point -> Point -> Maybe (Point, Point)
propose start elves elf
  | S.disjoint (S.fromList $ allNeighbs elf) elves = Nothing
  | otherwise =
      listToMaybe . mapMaybe go . take 4 . iterate getNextDir $ start
 where
  go :: Dir -> Maybe (Point, Point)
  go d =
    let
      checkPoints = S.fromList $ fmap (+ elf) (checks d)
     in
      if S.disjoint checkPoints elves
        then Just (elf, elf + dirPoint d)
        else Nothing

doRound :: Dir -> Set Point -> Set Point
doRound firstDir elves =
  M.foldlWithKey update elves shouldMove
 where
  proposed = foldl go M.empty elves
  go a e = case propose firstDir elves e of
    Just (from, to) -> M.insert from to a
    Nothing -> a
  proposedTargets :: Map Point Int
  proposedTargets = M.fromListWith (+) (zip (M.elems proposed) (repeat 1))
  shouldMove = M.filter (\v -> proposedTargets M.! v == 1) proposed
  update es f t = S.delete f . S.insert t $ es

showSet :: Set Point -> String
showSet s =
  [ if x == 11 then '\n' else if p `S.member` s then '#' else '.'
  | y <- [-2 .. 9]
  , x <- [-3 .. 11]
  , let p = V2 x y
  ]

getNextDir :: Dir -> Dir
getNextDir U = D
getNextDir D = L
getNextDir L = R
getNextDir R = U

solveA :: Set Point -> Int
solveA start =
  bbSize - S.size afterTen
 where
  rounds = fmap doRound . iterate getNextDir $ U
  afterTen = foldl' (\es rFn -> rFn es) start (take 10 rounds)
  (bbLo, bbHi) = case boundingBox' afterTen of
    Just x -> x
    Nothing -> error "Elves disappeared!?"
  bbSize = product (bbHi - bbLo + V2 1 1)

day23a :: Solution (Set Point) Int
day23a =
  Solution
    { sParse = Right . parseMap
    , sShow = show
    , sSolve = Right . solveA
    }

day23b :: Solution _ _
day23b = Solution{sParse = Right, sShow = show, sSolve = Right}
