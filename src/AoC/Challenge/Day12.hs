module AoC.Challenge.Day12 (
  day12a,
  day12b,
)
where

import AoC.Common.Graph (dijkstra, explore)
import AoC.Common.Point (cardinalNeighbs)
import AoC.Solution
import AoC.Util (maybeToEither)
import Data.Char (isLower, ord)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Linear (V2 (..))

type Point = V2 Int

parseMap :: String -> Map Point Char
parseMap =
  M.fromList
    . concatMap (\(y, xs) -> fmap (\(x, c) -> (V2 x y, c)) xs)
    . zip [0 ..]
    . fmap (zip [0 ..])
    . lines

type IterCtx = (Maybe Point, Maybe Point, Map Point Int)

findStartEnd :: Map Point Char -> Maybe (Point, Point, Map Point Int)
findStartEnd mp =
  case M.foldrWithKey go (Nothing, Nothing, M.empty) mp of
    (Just s, Just f, m) -> Just (s, f, m)
    _ -> Nothing
 where
  go :: Point -> Char -> IterCtx -> IterCtx
  go k a (s, f, m) = case a of
    l | isLower l -> (s, f, M.insert k (ord a - ord 'a') m)
    'S' -> (Just k, f, M.insert k 0 m)
    'E' -> (s, Just k, M.insert k 25 m)
    _ -> error $ "Invalid input letter: " ++ [a]

getNeighbs :: Map Point Int -> Point -> Map Point Int
getNeighbs m p =
  let pHeight = m M.! p
   in fmap (const 1) -- Just count the steps, no cost.
        . M.filter (<= (pHeight + 1))
        . M.fromList
        . mapMaybe (\n -> fmap (n,) m M.!? n)
        . cardinalNeighbs
        $ p

solveA :: (Point, Point, Map Point Int) -> Maybe Int
solveA (start, finish, m) =
  dijkstra (getNeighbs m) start (== finish)

day12a :: Solution (Point, Point, Map Point Int) Int
day12a =
  Solution
    { sParse = maybeToEither "no start or end" . findStartEnd . parseMap
    , sShow = show
    , sSolve = maybeToEither "no solve" . solveA
    }

getNeighbs' :: Map Point Int -> Point -> Map Point Int
getNeighbs' m p =
  let pHeight = m M.! p
   in fmap (const 1) -- Just count the steps, no cost.
        . M.filter (\h -> h >= pHeight - 1)
        . M.fromList
        . mapMaybe (\n -> fmap (n,) m M.!? n)
        . cardinalNeighbs
        $ p

day12b :: Solution (Point, Point, Map Point Int) Int
day12b =
  Solution
    { sParse = maybeToEither "no start or end" . findStartEnd . parseMap
    , sShow = show
    , sSolve = \(_, f, m) ->
        let allPaths = explore (getNeighbs' m) f
         in Right
              . minimum
              . mapMaybe (allPaths M.!?)
              . M.keys
              . M.filter (== 0)
              $ m
    }
