module AoC.Challenge.Day18 (
  day18a,
  day18b,
)
where

import AoC.Common.Graph (explore)
import AoC.Common.Point (cardinalNeighbs)
import AoC.Solution
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void (Void)
import Linear (V3 (..))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

type Point = V3 Int

pointParser :: MP.Parsec Void String Point
pointParser =
  V3
    <$> MPL.decimal
    <* MP.single ','
    <*> MPL.decimal
    <* MP.single ','
    <*> MPL.decimal

pointsParser :: MP.Parsec Void String [Point]
pointsParser = MP.sepBy pointParser MP.eol

parse :: String -> Either String [Point]
parse =
  first MP.errorBundlePretty . MP.parse pointsParser "day18"

getNeighbCount :: Set Point -> [Int] -> Point -> [Int]
getNeighbCount ps a =
  (: a) . length . filter id . fmap (`S.member` ps) . cardinalNeighbs

solveA :: [Point] -> Int
solveA ps =
  sum . fmap (6 -) . foldl' (getNeighbCount points) [] $ ps
 where
  points = S.fromList ps

boundingBox :: [V3 Int] -> (V3 Int, V3 Int)
boundingBox ps =
  let (V3 x y z) = head ps
   in toBB . foldl' go (x, y, z, x, y, z) $ tail ps
 where
  go (xMin, yMin, zMin, xMax, yMax, zMax) (V3 x y z) =
    (min xMin x, min yMin y, min zMin z, max xMax x, max yMax y, max zMax z)

  toBB (xMin, yMin, zMin, xMax, yMax, zMax) =
    (V3 xMin yMin zMin, V3 xMax yMax zMax)

day18a :: Solution [Point] Int
day18a = Solution{sParse = parse, sShow = show, sSolve = Right . solveA}

solveB :: [Point] -> Int
solveB ps =
  let points = S.fromList ps

      -- Get the bounding box of the remaining points.
      (bbMin, bbMax) = boundingBox ps

      -- Starting from just outside the bounding box, try and get to every
      -- air neighbour without using droplet spaces. If we can't get to it,
      -- it must be internal.
      bbMin'@(V3 xMin yMin zMin) = bbMin - V3 1 1 1
      (V3 xMax yMax zMax) = bbMax + V3 1 1 1

      possNeighbs :: V3 Int -> Map (V3 Int) Int
      possNeighbs =
        M.fromList
          . (`zip` repeat 0)
          . filter (\(V3 x y z) -> x >= xMin && y >= yMin && z >= zMin && x <= xMax && y <= yMax && z <= zMax)
          . filter (`S.notMember` points)
          . cardinalNeighbs

      -- Find all the external air tiles.
      air = explore possNeighbs bbMin'

      -- Check all the neighbours of droplets to see if they're external
      -- air tiles. If so, that's an external face.
      externalNeighbs =
        filter (`M.member` air)
          . filter (`S.notMember` points)
          . concatMap cardinalNeighbs
          $ ps
   in length externalNeighbs

day18b :: Solution [Point] Int
day18b = Solution{sParse = parse, sShow = show, sSolve = Right . solveB}
