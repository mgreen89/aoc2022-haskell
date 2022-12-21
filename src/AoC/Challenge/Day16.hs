module AoC.Challenge.Day16 (
  day16a,
  day16b,
)
where

import AoC.Common.Graph (explore)
import AoC.Solution
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.List (maximumBy, partition, sortBy)
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

getDistances :: Map String (Int, [String]) -> Map String [(String, Int)]
getDistances m =
  M.fromList
    . fmap (\v -> (v, sortBy (\(_, d1) (_, d2) -> compare d2 d1) . M.toList . getAllPaths v $ workingValves))
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
  { prev :: [String]
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

getMax :: (Metrics, [Ctx]) -> Int
getMax =
  (\c -> c.pTot)
    . maximumBy (\a b -> compare a.pTot b.pTot)
    . snd

solve :: Int -> Int -> Bool -> Map String (Int, [String]) -> (Metrics, [Ctx])
solve nAgents timeLimit stop inp =
  go
    M.empty
    []
    Ctx
      { selected = S.empty
      , opened = S.empty
      , pPerT = 0
      , pTot = 0
      , time = 0
      , agents = replicate nAgents Agent{prev = [], to = "AA", readyAt = 0}
      }
 where
  ds :: Map String [(String, Int)]
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
    let opened = S.union ctx.opened . S.fromList . fmap (head . (.prev)) $ as
    , -- Only count for pressure per time when it's actually opened - i.e.
    -- when just selected a new valve to visit.
    let pPerT = ctx.pPerT + (sum . fmap ((rates M.!) . head . (.prev)) $ as)
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
    | let dists = ds M.! a.to
    , let closed = filter (\(s, _) -> s `S.notMember` ctx.selected) dists
    , let inRange = filter (\(_, d) -> d < (timeLimit - ctx.time)) closed
    , let stay = (a.to, timeLimit)
    , let nexts = fmap (fmap (+ (ctx.time + 1))) inRange
    , (to, readyAt) <-
        if null nexts
          then [stay]
          else if stop then stay : nexts else nexts
    , readyAt <= timeLimit
    , let prev = a.to : a.prev
    ]

day16a :: Solution (Map String (Int, [String])) Int
day16a = Solution{sParse = parse, sShow = show, sSolve = Right . getMax . solve 1 30 False}

solveB :: Int -> Map String (Int, [String]) -> Int
solveB timeLimit inp =
  let (_, sols) = solve 1 timeLimit True inp
      bestSols = (M.toList $ M.fromListWith max [(S.delete "AA" s.opened, s.pTot) | s <- sols])
   in maximum
        [ xScore + yScore
        | (xOpened, xScore) <- bestSols
        , (yOpened, yScore) <- bestSols
        , S.disjoint xOpened yOpened
        ]

day16b :: Solution (Map String (Int, [String])) Int
day16b = Solution{sParse = parse, sShow = show, sSolve = Right . solveB 26}
