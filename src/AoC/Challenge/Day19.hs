module AoC.Challenge.Day19 (
  day19a,
  day19b,
)
where

import AoC.Common.Point (boundingBox)
import AoC.Solution
import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Maybe (mapMaybe)
import Data.Void (Void)
import GHC.Generics (Generic)
import Linear (V4 (..))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

data Resource = Ore | Clay | Obsidian | Geode
  deriving (Show, Eq, Ord, Generic, NFData)

allResources :: [Resource]
allResources = [Ore, Clay, Obsidian, Geode]

toVec :: Resource -> V4 Int
toVec = \case
  Ore -> V4 1 0 0 0
  Clay -> V4 0 1 0 0
  Obsidian -> V4 0 0 1 0
  Geode -> V4 0 0 0 1

(!) :: V4 a -> Resource -> a
(!) (V4 a b c d) r =
  case r of
    Ore -> a
    Clay -> b
    Obsidian -> c
    Geode -> d

type Recipe = V4 Int
type Blueprint = V4 Recipe
type Resources = V4 Int
type Robots = V4 Int

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
  pure (i, V4 ore clay obsidian geode)
 where
  recipe :: MP.Parsec Void String Recipe
  recipe = do
    ingredients <- flip MP.sepBy (MP.string " and ") $ do
      c <- MPL.decimal <* MP.space
      r <- resource
      case r of
        Ore -> pure $ V4 c 0 0 0
        Clay -> pure $ V4 0 c 0 0
        Obsidian -> pure $ V4 0 0 c 0
        Geode -> fail "Geode must not be a recipe ingredient"
    pure $ sum ingredients
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
  first MP.errorBundlePretty
    . MP.parse (MP.sepBy blueprintParser MP.space) "day19"

data State = State
  { time :: Int
  , resources :: Resources
  , robots :: Robots
  , factory :: Maybe Resource
  , skipped :: V4 Bool
  }
  deriving (Show)

initialState :: State
initialState =
  State
    { time = 0
    , resources = V4 0 0 0 0
    , robots = V4 1 0 0 0
    , factory = Nothing
    , skipped = V4 False False False False
    }

updateResources :: Resources -> Recipe -> Maybe Resources
updateResources resources recipe =
  let newResources = resources - recipe
   in if all (>= 0) newResources
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
          skipped = V4 False False False False
        }
  )
    <$> updateResources state.resources recipe
 where
  recipe = blueprint ! robot

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
  newResources = state.robots + state.resources
  newRobots = case state.factory of
    Just r -> toVec r + state.robots
    Nothing -> state.robots

type Cache = Int

tri :: Int -> Int
tri n = n * (n + 1) `div` 2

bestGeodes :: Int -> Blueprint -> Int
bestGeodes timeLimit blueprint =
  go 0 initialState
 where
  recipeMaximums :: V4 Int
  recipeMaximums = snd $ boundingBox blueprint

  maxPossScore :: State -> Int
  maxPossScore state =
    state.resources ! Geode
      + timeLeft * (state.robots ! Geode)
      + tri (timeLeft - 1)
   where
    timeLeft = timeLimit - state.time

  -- Get all the possible next states.
  getNext :: State -> [State]
  getNext state =
    reverse $ harvestOnly : mapMaybe (startFactory blueprint state') nextBuilds
   where
    state' :: State
    state' = tick state

    nextBuilds :: [Resource]
    nextBuilds =
      [ r
      | -- Try and build all the robots, this will be Nothing if there's not
      -- enough resources
      r <- allResources
      , -- Don't build a robot if it's been skipped.
      not (state.skipped ! r)
      , -- Don't build a robot for a resource of the number of such robots is
      -- already the maximum cost of that resource for any robot, otherwise
      -- that resource can't possibly be spent.
      -- Geode robots should always be built if possible!
      r == Geode || state.resources ! r < recipeMaximums ! r
      ]

    -- State where no robots are build.
    harvestOnly :: State
    harvestOnly = state'{skipped = fmap (== 1) . sum . fmap toVec $ nextBuilds}

  go :: Cache -> State -> Cache
  go cache state
    | state.time == timeLimit = max cache (state.resources ! Geode)
    | maxPossScore state <= cache = cache
    | otherwise = foldl' go cache (getNext state)

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
