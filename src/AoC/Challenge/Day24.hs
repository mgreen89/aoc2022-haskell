{-# LANGUAGE ScopedTypeVariables #-}

module AoC.Challenge.Day24 (
  day24a,
  day24b,
)
where

import AoC.Common.Point (Dir (..), boundingBox', cardinalNeighbs, dirPoint, manhattan)
import AoC.Solution
import Data.Map (Map)
import qualified Data.Map as M
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..))
import Safe (headMay)

type Point = V2 Int

type Input = (Point, Point, Point, Point, Set Point, Set Point, Set Point, Set Point)

parseInput :: String -> Input
parseInput inp =
  let
    initial =
      M.fromList
        . concat
        . zipWith (\y -> zipWith (\x -> (V2 x y,)) [0 ..]) [0 ..]
        . lines
        $ inp
    (lo@(V2 _ yLo), hi@(V2 _ yHi)) = case boundingBox' (M.keys initial) of
      Just x -> x
      Nothing -> error "no points!"
   in
    ( lo
    , hi
    , fst . head . M.toList . M.filterWithKey (\(V2 _ y) a -> y == yLo && a == '.') $ initial
    , fst . head . M.toList . M.filterWithKey (\(V2 _ y) a -> y == yHi && a == '.') $ initial
    , M.keysSet . M.filter (== '^') $ initial
    , M.keysSet . M.filter (== '>') $ initial
    , M.keysSet . M.filter (== 'v') $ initial
    , M.keysSet . M.filter (== '<') $ initial
    )

inBox :: (Point, Point, Point, Point) -> Point -> Bool
inBox (V2 xLo yLo, V2 xHi yHi, s, f) p@(V2 x y) =
  p == s || p == f || x > xLo && x < xHi && y > yLo && y < yHi

tick :: (Point, Point) -> (Set Point, Set Point, Set Point, Set Point) -> (Set Point, Set Point, Set Point, Set Point)
tick (V2 xLo yLo, V2 xHi yHi) (u, r, d, l) =
  (S.map (go U) u, S.map (go R) r, S.map (go D) d, S.map (go L) l)
 where
  go dir p =
    let p'@(V2 x' y') = dirPoint dir + p
        next = case dir of
          U -> if y' <= yLo then V2 x' (yHi - 1) else p'
          R -> if x' >= xHi then V2 (xLo + 1) y' else p'
          D -> if y' >= yHi then V2 x' (yLo + 1) else p'
          L -> if x' <= xLo then V2 (xHi - 1) y' else p'
     in next

insertIfBetter :: (Ord k, Ord p) => k -> p -> v -> OrdPSQ k p v -> OrdPSQ k p v
insertIfBetter k p x q = case PSQ.lookup k q of
  Nothing -> PSQ.insert k p x q
  Just (p', _)
    | p < p' -> PSQ.insert k p x q
    | otherwise -> q

-- | A* algorithm including a time heuristic.
aStar ::
  forall a n t.
  (Ord a, Num a, Ord n, Integral t) =>
  -- | Start time
  t ->
  -- | Heuristic
  (t -> n -> a) ->
  -- | Neighbours and costs
  (t -> n -> Map n a) ->
  -- | Start
  n ->
  -- | Destination
  n ->
  -- | Total cost if successful
  Maybe a
aStar startTime heuristic getNs start dest = go M.empty (PSQ.singleton (start, startTime) 0 0)
 where
  go :: Map (n, t) a -> OrdPSQ (n, t) a a -> Maybe a
  go visited unvisited = case headMay . M.toList . M.filterWithKey (\(p, _) _ -> p == dest) $ visited of
    Just (_, x) -> Just x
    Nothing -> uncurry go =<< step (visited, unvisited)

  step :: (Map (n, t) a, OrdPSQ (n, t) a a) -> Maybe (Map (n, t) a, OrdPSQ (n, t) a a)
  step (v, uv) = do
    ((currP, currT), _, currV, uv') <- PSQ.minView uv
    let v' = M.insert (currP, currT) currV v
    if currP == dest
      then -- Short circuit if the destination has the lowest cost.
        pure (v', uv')
      else pure (v', M.foldlWithKey' (handleNeighbour currV (currT + 1)) uv' (getNs (currT + 1) currP))
   where
    handleNeighbour :: a -> t -> OrdPSQ (n, t) a a -> n -> a -> OrdPSQ (n, t) a a
    handleNeighbour currCost t q n nCost
      | M.member (n, t) v = q
      | otherwise =
          insertIfBetter
            (n, t)
            (currCost + nCost + heuristic t n)
            (currCost + nCost)
            q

solveA :: Input -> Int
solveA (lo, hi, start, dest, u, r, d, l) =
  case aStar 0 heuristic neighbs start dest of
    Just x -> x
    Nothing -> error "no path!"
 where
  getBlizzards n = fmap blizz [0 ..] !! n
   where
    blizz :: Int -> (Set Point, Set Point, Set Point, Set Point)
    blizz 0 = (u, r, d, l)
    blizz x = tick (lo, hi) (blizz $ x - 1)
  neighbs t n =
    let bs = (\(w, x, y, z) -> S.unions [w, x, y, z]) $ getBlizzards t
     in M.fromList
          . flip zip (repeat 1)
          . filter (\p -> p `S.notMember` bs && inBox (lo, hi, start, dest) p)
          . filter (inBox (lo, hi, start, dest))
          $ n : cardinalNeighbs n
  heuristic t n =
    t + manhattan n dest

day24a :: Solution Input Int
day24a = Solution{sParse = Right . parseInput, sShow = show, sSolve = Right . solveA}

solveB :: Input -> Int
solveB (lo, hi, start, dest, u, r, d, l) =
  timeThere + timeBack + timeThereAgain
 where
  getBlizzards n = fmap blizz [0 ..] !! n
   where
    blizz :: Int -> (Set Point, Set Point, Set Point, Set Point)
    blizz 0 = (u, r, d, l)
    blizz x = tick (lo, hi) (blizz $ x - 1)
  neighbs t n =
    let bs = (\(w, x, y, z) -> S.unions [w, x, y, z]) $ getBlizzards t
     in M.fromList
          . flip zip (repeat 1)
          . filter (\p -> p `S.notMember` bs && inBox (lo, hi, start, dest) p)
          . filter (inBox (lo, hi, start, dest))
          $ n : cardinalNeighbs n
  heuristic t n =
    t + manhattan n dest

  timeThere = case aStar 0 heuristic neighbs start dest of
    Just x -> x
    Nothing -> error "no path there"

  timeBack = case aStar timeThere heuristic neighbs dest start of
    Just x -> x
    Nothing -> error "no path back"

  timeThereAgain = case aStar (timeThere + timeBack) heuristic neighbs start dest of
    Just x -> x
    Nothing -> error "no path there again"

day24b :: Solution Input Int
day24b = Solution{sParse = Right . parseInput, sShow = show, sSolve = Right . solveB}
