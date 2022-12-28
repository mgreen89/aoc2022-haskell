module AoC.Challenge.Day15 (
  day15a,
  day15b,
)
where

import AoC.Common.Point (manhattan)
import AoC.Solution
import AoC.Util (maybeToEither)
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Void (Void)
import Linear (V2 (..))
import Safe (headMay)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

type Point = V2 Int

-- Sensor at x=20, y=1: closest beacon is at x=15, y=3
sensorParser :: MP.Parsec Void String (Point, Point)
sensorParser = do
  MP.string "Sensor at "
  sx <- MP.string "x=" *> digit
  sy <- MP.string ", y=" *> digit
  MP.string ": closest beacon is at "
  bx <- MP.string "x=" *> digit
  by <- MP.string ", y=" *> digit
  pure (V2 sx sy, V2 bx by)
 where
  digit = MPL.signed MP.space MPL.decimal

parseSensors :: String -> Either String (Map Point Point)
parseSensors =
  fmap M.fromList
    . first MP.errorBundlePretty
    . MP.parse (MP.sepBy sensorParser MP.space) "day15"

solveA :: Int -> Map Point Point -> Int
solveA y m =
  foldl' (\a (mi, ma) -> ma - mi + a) 0 ranges - numBeacons
 where
  ranges :: [(Int, Int)]
  ranges = mergeRanges . mapMaybe getRange . M.toAscList $ m

  numBeacons :: Int
  numBeacons = S.size . S.fromList . M.elems $ m

  getRange :: (Point, Point) -> Maybe (Int, Int)
  getRange (s@(V2 sx sy), b) =
    let
      sensorRange = manhattan s b
      dy = abs (y - sy)
      dx = sensorRange - dy
     in
      if dy < sensorRange
        then Just (sx - dx, sx + dx)
        else Nothing

  mergeRanges :: [(Int, Int)] -> [(Int, Int)]
  mergeRanges =
    foldl go [] . sort
   where
    go :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
    go c (rMin, rMax) =
      case c of
        [] -> [(rMin, rMax)]
        (prevMin, prevMax) : rest ->
          if rMin <= prevMax
            then (prevMin, max prevMax rMax) : rest
            else (rMin, rMax) : rest

isTest :: Map Point Point -> Bool
isTest =
  all (\(s, b) -> max (maximum s) (maximum b) < 100) . M.toList

day15a :: Solution (Map Point Point) Int
day15a =
  Solution
    { sParse = parseSensors
    , sShow = show
    , sSolve = \i -> Right . solveA (if isTest i then 10 else 2000000) $ i
    }

tuningFreq :: Point -> Int
tuningFreq (V2 x y) = x * 4000000 + y

{-
The single point is either going to be on the boundary of the square
from (0, 0) - (maxCoord, maxCoord), which this solution ignores, or
_just_ outside the boundary of _two_ scanners.

The boundary of a scanner at (sx, sy) is made up of four lines -
two with gradient 1 and two with gradient -1, as follows:
  1.  y = x + (sy - sx + r + 1)
  2.  y = x + (sy - sx - r - 1)
  3.  y = (-x) + (sy + sx + r + 1)
  4.  y = (-x) + (sy + sx - r - 1)

Two lines y = x + a and y = -x - b intersect at ((b - a) / 2, (b + a) / 2).
One of these intersection points will be the scanner location.

We can also discard the following pairs:
  - Where (b - a) is an odd number - these don't share an integer-point
    intersection.
  - Where a >= b - these won't intersect in the region of interest.
-}
solveB :: Int -> Map Point Point -> Either String Int
solveB maxCoord sensors =
  fmap tuningFreq
    . maybeToEither "No point found"
    . headMay
    $ [ cand
      | a <- IS.toList aCoeffs
      , b <- IS.toList bCoeffs
      , even (b - a)
      , a < b
      , let cand = V2 ((b - a) `div` 2) ((b + a) `div` 2)
      , all (> 0) cand
      , all (< maxCoord) cand
      , all
          (\(sens, beac) -> manhattan cand sens > manhattan sens beac)
          (M.toAscList sensors)
      ]
 where
  (aCoeffs, bCoeffs) = M.foldrWithKey go (IS.empty, IS.empty) sensors

  go :: Point -> Point -> (IntSet, IntSet) -> (IntSet, IntSet)
  go s@(V2 x y) b (as, bs) =
    let r = manhattan s b
     in ( IS.insert (y - x + r + 1) . IS.insert (y - x - r - 1) $ as
        , IS.insert (y + x + r + 1) . IS.insert (y + x - r - 1) $ bs
        )

day15b :: Solution (Map Point Point) Int
day15b =
  Solution
    { sParse = parseSensors
    , sShow = show
    , sSolve = \i -> solveB (if isTest i then 20 else 4000000) i
    }
