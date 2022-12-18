module AoC.Challenge.Day16 (
  day16a,
  day16b,
)
where

import AoC.Common.Graph (explore)
import AoC.Solution
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.List (maximumBy, partition)
import Data.Map (Map)
import qualified Data.Map as M
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
    . fmap (\v -> (v, getAllPaths v workingValves))
    $ ("AA" : workingValves)
 where
  getAllPaths :: String -> [String] -> Map String Int
  getAllPaths src dsts =
    M.filterWithKey (\k _ -> k `elem` dsts) $ explore getNeighbs src

  getNeighbs :: String -> Map String Int
  getNeighbs = M.fromList . flip zip (repeat 1) . snd . (m M.!)

  workingValves :: [String]
  workingValves = M.keys . M.filter ((/= 0) . fst) $ m

data Agent = Agent
  { prev :: String
  , to :: String
  , readyAt :: Int
  }
  deriving (Show)

data Ctx = Ctx
  { selected :: Set String
  , opened :: Set String
  , pPerT :: Int
  , pTot :: Int
  , time :: Int
  , agents :: [Agent]
  }
  deriving (Show)

type Metrics = Map (Set String) Int

-- Get a metric to determine how "good" a path is.
-- This will be used to compare contexts with the same set of valves opened,
-- so needs to differentiate between them.
-- Use the maximum pressure relieved by the set of values as the metric.

getMetric :: Map String Int -> Int -> Ctx -> Int
getMetric rates timeLimit ctx =
  -- Max pressure possible relieved by the opened valves _and_ any selected
  -- valves once they're reached.
  let
    fromOpened = ctx.pTot + ctx.pPerT * (timeLimit - ctx.time)
   in
    fromOpened + foldl' go 0 ctx.agents
 where
  go :: Int -> Agent -> Int
  go s a =
    s + (rates M.! a.to) * (timeLimit - a.readyAt)

solve :: Int -> Int -> Map String (Int, [String]) -> Int
solve nAgents timeLimit inp =
  (\c -> c.pTot)
    . maximumBy (\a b -> compare a.pTot b.pTot)
    . snd
    $ go
      M.empty
      []
      Ctx
          { selected = S.empty
          , opened = S.empty
          , pPerT = 0
          , pTot = 0
          , time = 0
          , agents = replicate nAgents Agent{prev = "AA", to = "AA", readyAt = 0}
          }
 where
  ds :: Map String (Map String Int)
  ds = getDistances inp

  rates :: Map String Int
  rates = fmap fst inp

  go :: Metrics -> [Ctx] -> Ctx -> (Metrics, [Ctx])
  go metrics a ctx =
    if ctx.time == timeLimit
      then (metrics, ctx : a)
      else foldl' go' (metrics, []) (next metrics ctx)

  go' :: (Metrics, [Ctx]) -> Ctx -> (Metrics, [Ctx])
  go' (m, cs) c =
    -- If we've got this far, the metric must be the same or better
    -- so just insert it.
    let m' = M.insert c.selected (getMetric rates timeLimit c) m
        (m'', cs') = go m' cs c
    in (m'', cs ++ cs')

  next :: Metrics -> Ctx -> [Ctx]
  next metrics ctx =
    [ ctx'
    | let (readyAgents, enRouteAgents) =
            partition ((== ctx.time) . (.readyAt)) ctx.agents
    , as <- traverse (nextAgents ctx) readyAgents
    , -- Ensure all agents are going to different places!
    length as == S.size (S.fromList (fmap (.to) as))
    , -- Update the selected valves.
    let selected = S.union ctx.selected . S.fromList . fmap (.to) $ as
    , -- Update the opened valves.
    let opened = S.union ctx.opened . S.fromList . fmap (.prev) $ as
    , -- Only count for pressure per time when it's actually opened - i.e.
    -- when just selected a new valve to visit.
    let pPerT = ctx.pPerT + (sum . fmap ((rates M.!) . (.prev)) $ as)
    , let agents = as ++ enRouteAgents
    , -- Go until either reached time limit or an agent is ready.
    let nextReady = min timeLimit . minimum . fmap (.readyAt) $ agents
    , let dt = nextReady - ctx.time
    , -- Update the total pressure and time.
    let pTot = ctx.pTot + dt * pPerT
    , let time = ctx.time + dt
    , time <= timeLimit
    , let ctx' = Ctx{..}
    , -- Only let through if the metric is the best seen.
    let metric = getMetric rates timeLimit ctx'
    , case M.lookup selected metrics of
            Just x -> metric >= x
            Nothing -> True
    ]

  nextAgents :: Ctx -> Agent -> [Agent]
  nextAgents ctx a =
    [ Agent{..}
    | let prev = a.to
    , let dists = ds M.! prev
    , let closed = filter (`S.notMember` ctx.selected) $ M.keys dists
    , let inRange = filter ((< (timeLimit - ctx.time)) . (dists M.!)) closed
    , to <- if null inRange then [prev] else inRange
    , let readyAt = if to == prev then timeLimit else ctx.time + dists M.! to + 1
    , readyAt <= timeLimit
    ]

day16a :: Solution (Map String (Int, [String])) Int
day16a = Solution{sParse = parse, sShow = show, sSolve = Right . solve 1 30}

day16b :: Solution (Map String (Int, [String])) Int
day16b = Solution{sParse = parse, sShow = show, sSolve = Right . solve 2 26}
