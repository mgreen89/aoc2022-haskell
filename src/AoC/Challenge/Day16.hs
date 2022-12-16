{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day16 (
  day16a,
  day16b,
)
where

import AoC.Solution
import AoC.Util (dijkstra)
import Control.Monad (when)
import qualified Data.Array as A
import qualified Data.Array.MArray as A
import Data.Array.ST (STArray)
import qualified Data.Array.ST as A
import Data.Bifunctor (bimap, first)
import Data.Foldable (foldl', for_)
import Data.List (maximumBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

-- Sample input line:
-- Valve II has flow rate=0; tunnels lead to valves AA, JJ
parser :: MP.Parsec Void String (String, (Int, [String]))
parser = do
  MP.string "Valve "
  v <- valve
  MP.string " has flow rate="
  f <- MPL.decimal
  MP.choice
    [ MP.string "; tunnel leads to valve "
    , MP.string "; tunnels lead to valves "
    ]
  ts <- MP.sepBy valve (MP.string ", ")
  pure (v, (f, ts))
 where
  valve = MP.count 2 MP.upperChar

parse :: String -> Either String (Map String (Int, [String]))
parse =
  fmap M.fromList
    . traverse (first MP.errorBundlePretty . MP.parse parser "day16")
    . lines

getDistances :: Map String (Int, [String]) -> Map String (Map String Int)
getDistances m =
  M.fromList
    . fmap (\v -> (v, foldl' (go v) M.empty workingValves))
    $ ("AA" : workingValves)
 where
  -- Could change this to fully explore the space from each starting node
  -- once and look up the resulting lengths.
  go :: String -> Map String Int -> String -> Map String Int
  go src a dst
    | src == dst = a
    | otherwise = M.insert dst (fromJust $ dijkstra getNeighbs src dst) a

  getNeighbs :: String -> Map String Int
  getNeighbs = M.fromList . (flip zip) (repeat 1) . snd . (m M.!)

  workingValves :: [String]
  workingValves = M.keys . M.filter ((/= 0) . fst) $ m

type Ctx = (String, Set String, Int, Int, Int)

solveA :: Map String (Int, [String]) -> Int
solveA inp =
  (\(_, _, _, p, _) -> p)
    . maximumBy (\(_, _, _, p, _) (_, _, _, q, _) -> compare p q)
    $ go [] ("AA", S.empty, 0, 0, 0)
 where
  -- Never going to turn on a valve with flow rate zero.
  -- Build up a new map that doesn't include any of those valves, and
  -- has distances from working valve (and AA, for the first move) to every
  -- other valve pre-computed.
  ds :: Map String (Map String Int)
  ds = getDistances inp

  go :: [Ctx] -> Ctx -> [Ctx]
  go a x =
    case x of
      x@(_, _, _, p, 30) -> x : a
      x -> foldl go a (next x)

  next :: Ctx -> [Ctx]
  next (loc, open, pPerT, pTot, time) =
    [ (l, o, ppt, pt, t)
    | let dists = ds M.! loc
    , let closed = filter (`S.notMember` open) $ M.keys dists
    , let inRange = filter ((< (30 - time)) . (dists M.!)) closed
    , l <- if null inRange then [loc] else inRange
    , let o = S.insert l open
    , let dt = if l == loc then 30 - time else dists M.! l + 1
    , let ppt = if l == loc then pPerT else pPerT + fst (inp M.! l)
    , let pt = pTot + (pPerT * dt)
    , let t = time + dt
    , t <= 30
    ]

day16a :: Solution (Map String (Int, [String])) Int
day16a = Solution{sParse = parse, sShow = show, sSolve = Right . solveA}

day16b :: Solution _ _
day16b = Solution{sParse = Right, sShow = show, sSolve = Right}
