module AoC.Challenge.Day24 (
  day24a,
  day24b,
)
where

import AoC.Common.Point (boundingBox', cardinalNeighbs, inBoundingBox)
import AoC.Solution
import Data.Array (Array)
import qualified Data.Array as A
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..))

type Point = V2 Int

type DirBlizzards = Array Point Bool
type Blizzards = (DirBlizzards, DirBlizzards, DirBlizzards, DirBlizzards)

type Input = (Point, Point, Blizzards)

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
    ,
      ( A.array (V2 0 0, V2 (xHi - 1) (yHi - 1)) $ fmap (fmap (== '^')) noWalls
      , A.array (V2 0 0, V2 (xHi - 1) (yHi - 1)) $ fmap (fmap (== '>')) noWalls
      , A.array (V2 0 0, V2 (xHi - 1) (yHi - 1)) $ fmap (fmap (== 'v')) noWalls
      , A.array (V2 0 0, V2 (xHi - 1) (yHi - 1)) $ fmap (fmap (== '<')) noWalls
      )
    )

isBlizzard :: Blizzards -> Int -> Point -> Bool
isBlizzard (u, r, d, l) turn (V2 x y) =
  inU || inR || inD || inL
 where
  (_, V2 xHi yHi) = A.bounds u
  inU = u A.! V2 x ((y + turn) `mod` (yHi + 1))
  inR = r A.! V2 ((x - turn) `mod` (xHi + 1)) y
  inD = d A.! V2 x ((y - turn) `mod` (yHi + 1))
  inL = l A.! V2 ((x + turn) `mod` (xHi + 1)) y

bfs :: Int -> Input -> Int
bfs startTime (start, dest, blizzards@(u, _, _, _)) =
  go startTime (S.fromList [start])
 where
  go :: Int -> Set Point -> Int
  go t ps
    | dest `S.member` ps = t
    | otherwise =
        go (t + 1)
          . S.foldl (\s -> S.union s . neighbs t) S.empty
          $ ps

  neighbs :: Int -> Point -> Set Point
  neighbs t p =
    S.fromList
      . filter (isValid (t + 1))
      $ p : cardinalNeighbs p

  (lo, hi) = A.bounds u

  isValid :: Int -> Point -> Bool
  isValid t p
    | p == start = True
    | p == dest = True
    | otherwise =
        inBoundingBox (lo, hi) p && not (isBlizzard blizzards t p)

day24a :: Solution Input Int
day24a = Solution{sParse = Right . parseInput, sShow = show, sSolve = Right . bfs 0}

solveB :: Input -> Int
solveB (start, dest, blizzards) =
  timeThereAgain
 where
  timeThere = bfs 0 (start, dest, blizzards)
  timeBack = bfs timeThere (dest, start, blizzards)
  timeThereAgain = bfs timeBack (start, dest, blizzards)

day24b :: Solution Input Int
day24b = Solution{sParse = Right . parseInput, sShow = show, sSolve = Right . solveB}
