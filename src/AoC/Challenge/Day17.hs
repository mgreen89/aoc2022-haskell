{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day17 (
  day17a,
  day17b,
)
where

import AoC.Solution
import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Data.Foldable (maximumBy)
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Linear (V2 (..))
import Text.Read (readEither)

type Point = V2 Int

data Gust = L | R deriving (Generic, NFData)

instance Show Gust where
  show L = "<"
  show R = ">"

instance Read Gust where
  readsPrec _ ('<' : rest) = [(L, rest)]
  readsPrec _ ('>' : rest) = [(R, rest)]
  readsPrec _ ('L' : rest) = [(L, rest)]
  readsPrec _ ('R' : rest) = [(R, rest)]
  readsPrec _ _ = []

toDir :: Gust -> Point
toDir L = V2 (-1) 0
toDir R = V2 1 0

type Shape = [Point]

-- Shapes of rocks.  Bottom left corner of a rectangle entirely containing
-- the rock is at (0, 0), y increases going up.
rocks :: [Shape]
rocks =
  [ [V2 0 0, V2 1 0, V2 2 0, V2 3 0]
  , [V2 0 1, V2 1 0, V2 1 1, V2 1 2, V2 2 1]
  , [V2 0 0, V2 1 0, V2 2 0, V2 2 1, V2 2 2]
  , [V2 0 0, V2 0 1, V2 0 2, V2 0 3]
  , [V2 0 0, V2 1 0, V2 0 1, V2 1 1]
  ]

fall :: ([Gust], Int, Set Point) -> Shape -> ([Gust], Int, Set Point)
fall (gusts, highest, room) rock =
  let start = fmap (+ V2 2 (highest + 4)) rock
      Left (final, gusts') = foldM go (start, gusts) $ [0 ..]
      room' = S.union room . S.fromList $ final
      highest' =
        (\(V2 _ y) -> y)
          . maximumBy (\(V2 _ a) (V2 _ b) -> compare a b)
          . S.toList
          $ room'
   in (gusts', highest', room')
 where
  go :: (Shape, [Gust]) -> Int -> Either (Shape, [Gust]) (Shape, [Gust])
  go (s, gs) _ =
    let pushAttempt = fmap (+ toDir (head gs)) s
        pushed = if collides room pushAttempt then s else pushAttempt
        dropAttempt = fmap (+ V2 0 (-1)) pushed
     in if collides room dropAttempt
          then Left (pushed, tail gs)
          else Right (dropAttempt, tail gs)

  collides :: Set Point -> Shape -> Bool
  collides r =
    any (\p@(V2 x y) -> p `S.member` r || y <= 0 || x < 0 || x > 6)

solveA :: [Gust] -> Int
solveA gusts =
  let
    endlessGusts = concat $ repeat gusts
    endlessRocks = concat $ repeat rocks
  in
  (\(_, h, _) -> h)
  . (!! 2022)
  . scanl fall (endlessGusts, 0, S.empty)
  $ endlessRocks

day17a :: Solution [Gust] Int
day17a =
  Solution
    { sParse = traverse (readEither . (: []))
    , sShow = show
    , sSolve = Right . solveA
    }

day17b :: Solution _ _
day17b = Solution{sParse = Right, sShow = show, sSolve = Right}
