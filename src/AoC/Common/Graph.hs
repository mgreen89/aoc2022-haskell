{-# LANGUAGE ScopedTypeVariables #-}

module AoC.Common.Graph (
  aStar,
  dijkstra,
  explore,
) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ

insertIfBetter :: (Ord k, Ord p) => k -> p -> v -> OrdPSQ k p v -> OrdPSQ k p v
insertIfBetter k p x q = case PSQ.lookup k q of
  Nothing -> PSQ.insert k p x q
  Just (p', _)
    | p < p' -> PSQ.insert k p x q
    | otherwise -> q

--------------------------------------
-- Path Finding
--------------------------------------

aStar ::
  forall a n.
  (Ord a, Num a, Ord n) =>
  -- | Heuristic
  (n -> a) ->
  -- | Neighbours and costs
  (n -> Map n a) ->
  -- | Start
  n ->
  -- | Destination
  (n -> Bool) ->
  -- | Total cost if successful
  Maybe a
aStar heuristic getNs start isDest =
  (\(n, _, _) -> n) <$> go (M.empty, PSQ.singleton start 0 0)
 where
  go :: (Map n a, OrdPSQ n a a) -> Maybe (a, Map n a, OrdPSQ n a a)
  go (v, uv) = do
    (currP, _, currV, uv') <- PSQ.minView uv
    let v' = M.insert currP currV v
    if isDest currP
      then pure (currV, v', uv')
      else go (v', M.foldlWithKey' (handleNeighbour currV) uv' (getNs currP))
   where
    handleNeighbour :: a -> OrdPSQ n a a -> n -> a -> OrdPSQ n a a
    handleNeighbour currCost q n nCost
      | M.member n v = q
      | otherwise =
          insertIfBetter
            n
            (currCost + nCost + heuristic n)
            (currCost + nCost)
            q

dijkstra ::
  forall a n.
  (Ord a, Num a, Ord n) =>
  -- | Neighbours and costs
  (n -> Map n a) ->
  -- | Start
  n ->
  -- | Destination
  (n -> Bool) ->
  -- | Total cost if successful
  Maybe a
dijkstra = aStar (const 0)

{- | Fully explore a graph using a dijkstra-like algorithm.
 Returns a map of distances from the start node for every found node.
-}
explore ::
  forall a n.
  (Ord a, Num a, Ord n) =>
  -- | Neighbours and costs
  (n -> Map n a) ->
  -- | Start
  n ->
  -- | Total cost if successful
  Map n a
explore getNs start =
  go M.empty (PSQ.singleton start 0 0)
 where
  go :: Map n a -> OrdPSQ n a a -> Map n a
  go visited unvisited =
    case step (visited, unvisited) of
      Just (v, uv) -> go v uv
      Nothing -> visited

  step :: (Map n a, OrdPSQ n a a) -> Maybe (Map n a, OrdPSQ n a a)
  step (v, uv) = do
    (currP, _, currV, uv') <- PSQ.minView uv
    let v' = M.insert currP currV v
    pure (v', M.foldlWithKey' (handleNeighbour currV) uv' (getNs currP))
   where
    handleNeighbour :: a -> OrdPSQ n a a -> n -> a -> OrdPSQ n a a
    handleNeighbour currCost q n nCost
      | M.member n v = q
      | otherwise =
          insertIfBetter
            n
            (currCost + nCost)
            (currCost + nCost)
            q
