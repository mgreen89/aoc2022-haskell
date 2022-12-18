module AoC.Common.Point (
  cardinalNeighbs,
  allNeighbs,
  manhattan,
  boundingBox,
  parse2dMap,
) where

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Semigroup
import Data.Semigroup.Foldable
import Linear (Additive, V2 (..), basis, negated)
import Text.Read (readEither)

-- Cardinal neighbour moves.
cardinalDiffs :: (Traversable t, Additive t, Num a) => [t a]
cardinalDiffs = fmap negated basis <> basis

-- | Get the cardinal neighbours (i.e. excluding diagonals) of a vector.
cardinalNeighbs ::
  (Traversable t, Additive t, Applicative t, Num a) =>
  t a ->
  [t a]
cardinalNeighbs p =
  [liftA2 (+) p delta | delta <- cardinalDiffs]

-- All neighbour moves.
allDiffs :: (Traversable t, Applicative t, Num a) => [t a]
allDiffs =
  -- Take the tail as the first delta is the zero vector.
  tail $ sequence (pure [0, -1, 1])

-- | Get all neighbours (including diagonals) of a vector.
allNeighbs :: (Traversable t, Applicative t, Num a) => t a -> [t a]
allNeighbs p =
  [liftA2 (+) p delta | delta <- allDiffs]

-- | Get the manhattan distance between two vectors.
manhattan :: (Foldable f, Num a, Num (f a)) => f a -> f a -> a
manhattan x y = sum . abs $ x - y

-- | Get the bounding box of a collection of points.
boundingBox :: (Foldable1 f, Applicative g, Ord a) => f (g a) -> (g a, g a)
boundingBox =
  (\(Ap mi, Ap ma) -> (getMin <$> mi, getMax <$> ma))
    . foldMap1 (\p -> (Ap (Min <$> p), Ap (Max <$> p)))

-- | Parse String data into a Map
parse2dMap :: String -> Either String (Map (V2 Int) Int)
parse2dMap = fmap createMap . traverse (traverse (readEither . pure)) . lines
 where
  createMap :: [[Int]] -> Map (V2 Int) Int
  createMap =
    M.fromList
      . concat
      . zipWith
        (\y -> zipWith (\x -> (V2 x y,)) [0 ..])
        [0 ..]
