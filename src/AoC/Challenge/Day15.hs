{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day15 (
  day15a,
  day15b,
)
where

import AoC.Solution
import Data.Bifunctor (first)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void (Void)
import Linear (V2 (..))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

import Debug.Trace

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
    . traverse (first MP.errorBundlePretty . MP.parse sensorParser "day15")
    . lines

manhattan :: Point -> Point -> Int
manhattan a b =
  sum . fmap abs $ (b - a)

solveA :: Map Point Point -> Int
solveA =
  S.size . M.foldrWithKey go S.empty
 where
  go :: Point -> Point -> Set Point -> Set Point
  go s b a =
    S.union a . S.fromList . getPoints 2000000 s $ b

  getPoints :: Int -> Point -> Point -> [Point]
  getPoints y s@(V2 sx sy) b =
    let
      d = manhattan s b
      toL = abs (y - sy)
     in
      if abs toL >= d
        then []
        else
          let xDiff = d - toL
           in [ V2 x y | x <- [sx - xDiff .. sx + xDiff], V2 x y /= b ]

day15a :: Solution (Map Point Point) Int
day15a =
  Solution
    { sParse = parseSensors
    , sShow = show
    , sSolve = Right . solveA
    }

day15b :: Solution _ _
day15b = Solution{sParse = Right, sShow = show, sSolve = Right}
