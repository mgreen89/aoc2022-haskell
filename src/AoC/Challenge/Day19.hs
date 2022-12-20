module AoC.Challenge.Day19 (
  day19a,
  day19b,
)
where

import AoC.Solution
import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust)
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

data Resource = Ore | Clay | Obsidian | Geode
  deriving (Show, Eq, Ord, Generic, NFData)

allResources :: [Resource]
allResources = [Ore, Clay, Obsidian, Geode]

type Recipe = Map Resource Int
type Blueprint = Map Resource Recipe
type Resources = Map Resource Int
type Robots = Map Resource Int

blueprintParser :: MP.Parsec Void String (Int, Blueprint)
blueprintParser = do
  MP.string "Blueprint "
  i <- MPL.decimal <* MP.string ":" <* MP.space
  MP.string "Each ore robot costs "
  ore <- recipe <* MP.string "." <* MP.space
  MP.string "Each clay robot costs "
  clay <- recipe <* MP.string "." <* MP.space
  MP.string "Each obsidian robot costs "
  obsidian <- recipe <* MP.string "." <* MP.space
  MP.string "Each geode robot costs "
  geode <- recipe <* MP.string "."
  pure (i, M.fromList [(Ore, ore), (Clay, clay), (Obsidian, obsidian), (Geode, geode)])
 where
  recipe :: MP.Parsec Void String Recipe
  recipe = do
    ingredients <- flip MP.sepBy (MP.string " and ") $ do
      c <- MPL.decimal <* MP.space
      r <- resource
      pure (r, c)
    pure $ M.fromList ingredients
  resource :: MP.Parsec Void String Resource
  resource =
    MP.choice
      [ Ore <$ MP.string "ore"
      , Clay <$ MP.string "clay"
      , Obsidian <$ MP.string "obsidian"
      , Geode <$ MP.string "geode"
      ]

parse :: String -> Either String [(Int, Blueprint)]
parse =
  first MP.errorBundlePretty . MP.parse (MP.sepBy blueprintParser MP.space) "day19"

data State = State
  { time :: Int
  , resources :: Resources
  , robots :: Robots
  , factory :: Maybe Resource
  , skippedBuild :: [Bool]
  }
  deriving (Show)

initialState :: State
initialState =
  State
    { time = 0
    , resources = M.fromList $ zip allResources (repeat 0)
    , robots = M.unions [M.singleton Ore 1, M.fromList $ zip allResources (repeat 0)]
    , factory = Nothing
    , skippedBuild = fmap (const False) allResources
    }

updateResources :: Resources -> Recipe -> Maybe Resources
updateResources resources recipe =
  let newResources = M.unionWith (-) resources recipe
   in if all (>= 0) (M.elems newResources)
        then Just newResources
        else Nothing

startFactory :: Blueprint -> State -> Resource -> Maybe State
startFactory blueprint state robot =
  ( \rs ->
      state
        { resources = rs
        , factory = Just robot
        , -- Now the factory has been started, allow all robots to
          -- be considered for the next round.
          skippedBuild = fmap (const False) allResources
        }
  )
    <$> updateResources state.resources recipe
 where
  recipe = blueprint M.! robot

-- Timer tick the state:
--  - Gather resources with all existing robots.
--  - Create a new robot if the factory is working.
tick :: State -> State
tick state =
  state
    { time = state.time + 1
    , resources = newResources
    , robots = newRobots
    , factory = Nothing
    }
 where
  newResources = M.unionWith (+) state.robots state.resources
  newRobots = case state.factory of
    Just r -> M.adjust (+ 1) r state.robots
    Nothing -> state.robots

type Cache = Int

tri :: Int -> Int
tri n = n * (n + 1) `div` 2

bestGeodes :: Int -> Blueprint -> Int
bestGeodes timeLimit blueprint =
  fst . go (0, []) $ initialState
 where
  recipeMaximums :: Map Resource Int
  recipeMaximums =
    M.unionsWith max . fmap snd . M.toList $ blueprint

  maxPossScore :: State -> Int
  maxPossScore state =
    state.resources M.! Geode
      + timeLeft * M.findWithDefault 0 Geode state.robots
      + tri timeLeft
   where
    timeLeft = timeLimit - state.time

  -- Get all the possible next states.
  getNext :: State -> [State]
  getNext state =
    catMaybes
      [ build
      | -- Try and build all the robots, this will be Nothing if there's not
      -- enough resources
      let tickedState = tick state
      , let allBuilds = fmap (startFactory blueprint tickedState) allResources
      , let allCanBuild = fmap isJust allBuilds
      , -- Don't build a robot if it's been skipped.
      let allMayBuild = fmap (\(can, skipped) -> can && not skipped) (zip allCanBuild state.skippedBuild)
      , -- Don't build a robot for a resource of the number of such robots is
      -- already the maximum cost of that resource for any robot, otherwise
      -- that resource can't possibly be spent.
      -- Geode robots should always be built if possible!
      let allShouldBuild = fmap (\r -> state.resources M.! r < recipeMaximums M.! r) (take 3 allResources) ++ [True]
      , -- The state if no robots are built this turn.
      -- Any robots that are possible to built but haven't been should be
      -- marked as skipped so they're not attempted until after the next robot
      -- is built.
      let harvestOnly = tickedState{skippedBuild = uncurry (||) <$> zip allCanBuild state.skippedBuild}
      , -- Try to build in the order Geode, Obsidian, Clay, Ore, None.
      (build, shouldBuild, mayBuild) <- reverse $ (Just harvestOnly, True, True) : zip3 allBuilds allShouldBuild allMayBuild
      , shouldBuild && mayBuild
      ]

  go :: (Cache, [State]) -> State -> (Cache, [State])
  go (cache, a) state
    | state.time == timeLimit = (max cache (state.resources M.! Geode), state : a)
    | maxPossScore state <= cache = (cache, [])
    | otherwise = foldl' go' (cache, a) (getNext state)
   where
    go' :: (Cache, [State]) -> State -> (Cache, [State])
    go' (c, ss) s =
      let (c', ss') = go (c, ss) s
       in (c', ss ++ ss')

day19a :: Solution [(Int, Blueprint)] Int
day19a =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve = Right . sum . fmap (\(i, bp) -> i * bestGeodes 24 bp)
    }

day19b :: Solution [(Int, Blueprint)] Int
day19b =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve = Right . product . fmap (bestGeodes 32 . snd) . take 3
    }
