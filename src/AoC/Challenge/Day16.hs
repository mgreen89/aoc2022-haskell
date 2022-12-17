module AoC.Challenge.Day16 (
  day16a,
  day16b,
)
where

import AoC.Solution
import AoC.Util (explore)
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

import Debug.Trace

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
  { opened :: Set String
  , pPerT :: Int
  , pTot :: Int
  , time :: Int
  , agents :: [Agent]
  }
  deriving (Show)

-- Get a metric to determine how "good" a path is.
-- This will be used to compare contexts with the same set of valves opened,
-- so needs to differentiate between them.
-- Use the maximum pressure relieved by the set of values as the metric.
getMetric :: Int -> Ctx -> Int
getMetric timeLimit ctx =
  -- Max pressure possible relieved by the opened valves.
  ctx.pTot + ctx.pPerT * (timeLimit - ctx.time)

solveB :: Int -> Int -> Map String (Int, [String]) -> Int
solveB nAgents timeLimit inp =
  (\c -> c.pTot)
    . maximumBy (\a b -> compare a.pTot b.pTot)
    $ go
      []
      Ctx
        { opened = S.empty
        , pPerT = 0
        , pTot = 0
        , time = 0
        , agents = replicate nAgents Agent{prev = "AA", to = "AA", readyAt = 0}
        }
 where
  ds :: Map String (Map String Int)
  ds = getDistances inp

  go :: [Ctx] -> Ctx -> [Ctx]
  go a c =
    if c.time == timeLimit
      then c : a
      else foldl go a (next c)

  next :: Ctx -> [Ctx]
  next ctx =
    [ Ctx{..}
    | let readyAgents = filter ((== ctx.time) . (.readyAt)) ctx.agents
    , let enRouteAgents = filter ((/= ctx.time) . (.readyAt)) ctx.agents
    , as <- traverse (nextAgents ctx) readyAgents
      -- Ensure all agents are going to different places!
    , length as == S.size (S.fromList (fmap (.to) as))
      -- Count as opened as soon as selected so it's not selected again.
    , let opened = S.union ctx.opened . S.fromList . fmap (.to) $ as
      -- Only count for pressure per time when it's actually opened - i.e.
      -- when just selected a new valve to visit.
    , let pPerT = ctx.pPerT + (sum . fmap (fst . (inp M.!) . (.prev)) $ as)
    , let agents = as ++ enRouteAgents
      -- Go until either reached time limit or an agent is ready.
    , let nextReady = min timeLimit . minimum . fmap (.readyAt) $ agents
    , let dt = nextReady - ctx.time
      -- Update the total pressure and time.
    , let pTot = ctx.pTot + dt * pPerT
    , let time = ctx.time + dt
    , time <= timeLimit
    ]

  nextAgents :: Ctx -> Agent -> [Agent]
  nextAgents ctx a =
    [ Agent{..}
    | let prev = a.to
    , let dists = ds M.! prev
    , let closed = filter (`S.notMember` ctx.opened) $ M.keys dists
    , let inRange = filter ((< (timeLimit - ctx.time)) . (dists M.!)) closed
    , to <- if null inRange then [prev] else inRange
    , let readyAt = if to == prev then timeLimit else ctx.time + dists M.! to + 1
    , readyAt <= timeLimit
    ]

day16a :: Solution (Map String (Int, [String])) Int
day16a = Solution{sParse = parse, sShow = show, sSolve = Right . solveB 1 30}

day16b :: Solution (Map String (Int, [String])) Int
day16b = Solution{sParse = parse, sShow = show, sSolve = Right . solveB 2 26}
