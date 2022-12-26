module AoC.Challenge.Day15 (
  day15a,
  day15b,
)
where

import AoC.Common.Point (manhattan)
import AoC.Solution
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Void (Void)
import Linear (V2 (..))
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

solveB :: Int -> Map Point Point -> Int
solveB maxCoord bs =
  tuningFreq
    . head
    $ [ cand
      | (b@(V2 bx by), s) <- M.toList bs
      , let d = manhattan b s + 1
      , dx <- [(-d) .. d]
      , let x = bx + dx
      , x >= 0 && x <= maxCoord
      , dy <- [-(d - dx), d - dx]
      , let y = by + dy
      , y >= 0 && y <= maxCoord
      , let cand = V2 x y
      , all (\(b', s') -> manhattan b' cand > manhattan b' s') $ M.toList bs
      ]

day15b :: Solution (Map Point Point) Int
day15b =
  Solution
    { sParse = parseSensors
    , sShow = show
    , sSolve = Right . solveB 4000000
    }
