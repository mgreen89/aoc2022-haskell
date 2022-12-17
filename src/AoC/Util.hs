{-# LANGUAGE ScopedTypeVariables #-}

module AoC.Util (
  Point,
  cardinalNeighbours,
  allNeighbours,
  parseMap,
  freqs,
  strip,
  stripNewlines,
  eitherToMaybe,
  maybeToEither,
  listTo2Tuple,
  aStar,
  dijkstra,
  explore,
  withColor,
)
where

import Control.Applicative (
  Alternative,
  empty,
  liftA2,
 )
import Control.Monad.Except (
  MonadError,
  throwError,
 )
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import qualified Data.Text as T
import Linear.V2 (V2 (..))
import Linear.Vector (basisFor)
import qualified System.Console.ANSI as ANSI
import Text.Read (readEither)

--------------------------------------
-- Map helpers (2D, 3D, etc...)
--------------------------------------

type Point = V2 Int

-- | Get all neighbours (including diagonals) of a vector.
allNeighbours :: (Traversable t, Applicative t, Num a) => t a -> [t a]
allNeighbours p =
  -- Take the tail as the first delta is the zero vector.
  tail [liftA2 (+) p delta | delta <- sequence (pure [0, -1, 1])]

-- | Get the cardinal neighbours (i.e. excluding diagonals) of a vector.
cardinalNeighbours :: (Traversable t, Applicative t, Num a) => t a -> [t a]
cardinalNeighbours p =
  [ liftA2 (+) p delta
  | delta <- basisFor p <> fmap (fmap (fmap negate)) basisFor p
  ]

-- | Parse String data into a Map
parseMap :: String -> Either String (Map Point Int)
parseMap = fmap createMap . traverse (traverse (readEither . pure)) . lines
 where
  createMap :: [[Int]] -> Map Point Int
  createMap =
    M.fromList
      . concat
      . zipWith
        (\y -> zipWith (\x -> (V2 x y :: Point,)) [0 ..])
        [0 ..]

--------------------------------------
-- Basic statistics
--------------------------------------

-- | Create a frequency map.
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = M.fromListWith (+) . fmap (,1) . toList

--------------------------------------
-- Parsing and string handling
--------------------------------------

-- | Strip trailing and leading whitepsace
strip :: String -> String
strip = T.unpack . T.strip . T.pack

-- | Strip trailing newlines
stripNewlines :: String -> String
stripNewlines = reverse . dropWhile (== '\n') . reverse

--------------------------------------
-- Conversions
--------------------------------------

{- | Convert an 'Either' into a 'Maybe' (or any other 'Alternative' instance),
 dropping the error value.
-}
eitherToMaybe :: Alternative m => Either e a -> m a
eitherToMaybe = either (const empty) pure

{- | Conver a 'Maybe' into an 'Either' (or any other 'MonadError' instance),
 using the provided error value if necessary.
-}
maybeToEither :: MonadError e m => e -> Maybe a -> m a
maybeToEither e = maybe (throwError e) pure

-- | Safely convert a list to a 2-tuple of the elements.
listTo2Tuple :: [a] -> Either String (a, a)
listTo2Tuple [a, b] = Right (a, b)
listTo2Tuple _ = Left "Not a 2-elem list"

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
  n ->
  -- | Total cost if successful
  Maybe a
aStar heuristic getNs start dest = go M.empty (PSQ.singleton start 0 0)
 where
  go :: Map n a -> OrdPSQ n a a -> Maybe a
  go visited unvisited = case M.lookup dest visited of
    Just x -> Just x
    Nothing -> uncurry go =<< step (visited, unvisited)

  step :: (Map n a, OrdPSQ n a a) -> Maybe (Map n a, OrdPSQ n a a)
  step (v, uv) = do
    (currP, _, currV, uv') <- PSQ.minView uv
    let v' = M.insert currP currV v
    if currP == dest
      then -- Short circuit if the destination has the lowest cost.
        pure (v', uv')
      else pure (v', M.foldlWithKey' (handleNeighbour currV) uv' (getNs currP))
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
  n ->
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

--------------------------------------
-- Output
--------------------------------------

-- | Run som IO with the specified foreground colour and intensity.
withColor :: ANSI.ColorIntensity -> ANSI.Color -> IO () -> IO ()
withColor ci c io = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ci c]
  io
  ANSI.setSGR [ANSI.Reset]
