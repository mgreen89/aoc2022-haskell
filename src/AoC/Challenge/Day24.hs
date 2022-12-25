module AoC.Challenge.Day24 (
  day24a,
  day24b,
)
where

import AoC.Common.Point (inBoundingBox, boundingBox', cardinalNeighbs, manhattan)
import AoC.Common.Graph (aStar)
import AoC.Solution
import Data.Array (Array)
import qualified Data.Array as A
import Data.Map (Map)
import qualified Data.Map as M
import Linear (V2 (..))

type Point = V2 Int

type Blizzards = Array Point Bool

type Input = (Point, Point, Blizzards, Blizzards, Blizzards, Blizzards)

parseInput :: String -> Input
parseInput inp =
  let
    initial =
      M.fromList
        . concat
        . zipWith (\y -> zipWith (\x -> (V2 x y,)) [(-1) ..]) [(-1) ..]
        . lines
        $ inp
    (V2 xLo yLo, V2 xHi yHi) = case boundingBox' (M.keys initial) of
      Just x -> x
      Nothing -> error "no points!"

    -- Drop all the walls.
    noWalls = M.toAscList $ M.filterWithKey (\(V2 x y) _ -> x > xLo && x < xHi && y > yLo && y < yHi) initial
   in
    ( fst . head . M.toList . M.filterWithKey (\(V2 _ y) a -> y == yLo && a == '.') $ initial
    , fst . head . M.toList . M.filterWithKey (\(V2 _ y) a -> y == yHi && a == '.') $ initial
    , A.array (V2 0 0, V2 (xHi - 1) (yHi - 1)) $ fmap (fmap (== '^')) noWalls
    , A.array (V2 0 0, V2 (xHi - 1) (yHi - 1)) $ fmap (fmap (== '>')) noWalls
    , A.array (V2 0 0, V2 (xHi - 1) (yHi - 1)) $ fmap (fmap (== 'v')) noWalls
    , A.array (V2 0 0, V2 (xHi - 1) (yHi - 1)) $ fmap (fmap (== '<')) noWalls
    )

isBlizzard :: (Blizzards, Blizzards, Blizzards, Blizzards) -> Int -> Point -> Bool
isBlizzard (u, r, d, l) turn (V2 x y) =
  inU || inR || inD || inL
 where
  (_, V2 xHi yHi) = A.bounds u
  inU = u A.! V2 x ((y + turn) `mod` (yHi + 1))
  inR = r A.! V2 ((x - turn) `mod` (xHi + 1)) y
  inD = d A.! V2 x ((y - turn) `mod` (yHi + 1))
  inL = l A.! V2 ((x + turn) `mod` (xHi + 1)) y

solveA :: Input -> Int
solveA (start, dest, u, r, d, l) =
  case aStar heuristic neighbs (start, 0) ((== dest) . fst) of
    Just x -> x
    Nothing -> error "no path!"
 where
  (lo, hi) = A.bounds u

  neighbs :: (Point, Int) -> Map (Point, Int) Int
  neighbs (n, t) =
    M.fromList
      . flip zip (repeat 1)
      . flip zip (repeat (t + 1))
      . filter (isValid (t + 1))
      $ n : cardinalNeighbs n

  isValid :: Int -> Point -> Bool
  isValid t p
    | p == start = True
    | p == dest = True
    | otherwise =
      inBoundingBox (lo, hi) p && not (isBlizzard (u, r, d, l) t p)

  heuristic :: (Point, Int) -> Int
  heuristic (n, t) =
    t + manhattan n dest

day24a :: Solution Input Int
day24a = Solution{sParse = Right . parseInput, sShow = show, sSolve = Right . solveA}

solveB :: Input -> Int
solveB (start, dest, u, r, d, l) =
  timeThere + timeBack + timeThereAgain
 where
  (lo, hi) = A.bounds u
  neighbs (n, t) =
    M.fromList
      . flip zip (repeat 1)
      . flip zip (repeat (t + 1))
      . filter (isValid (t + 1))
      $ n : cardinalNeighbs n

  heuristic (n, t) =
    t + manhattan n dest

  isValid :: Int -> Point -> Bool
  isValid t p
    | p == start = True
    | p == dest = True
    | otherwise =
      inBoundingBox (lo, hi) p && not (isBlizzard (u, r, d, l) t p)

  isStart = (== start) . fst
  isDest = (== dest) . fst

  timeThere = case aStar heuristic neighbs (start, 0) isDest of
    Just x -> x
    Nothing -> error "no path there"

  timeBack = case aStar heuristic neighbs (dest, timeThere) isStart of
    Just x -> x
    Nothing -> error "no path back"

  timeThereAgain = case aStar heuristic neighbs (start, timeThere + timeBack) isDest of
    Just x -> x
    Nothing -> error "no path there again"

day24b :: Solution Input Int
day24b = Solution{sParse = Right . parseInput, sShow = show, sSolve = Right . solveB}
