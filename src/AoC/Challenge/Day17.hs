{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AoC.Challenge.Day17 (
  day17a,
  day17b,
)
where

import AoC.Solution
import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Data.List (foldl', scanl')
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
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

maxY :: [Point] -> Int
maxY = foldl' (\m (V2 _ y) -> max m y) 0

data Ctx = Ctx
  { gusts :: [(Int, Gust)]
  , highest :: Int
  , room :: Set Point
  }

cleanupRoom :: Int -> Set Point -> Set Point
cleanupRoom nLines room =
  let highest = maxY $ S.toList room
      dy = highest - nLines
      shifted =
        if dy > 0
          then S.mapMonotonic (+ V2 0 (-dy)) room
          else room
   in S.filter (\(V2 _ y) -> y >= 0) shifted

fall :: Int -> Ctx -> Shape -> Ctx
fall nHistory ctx rock =
  let highestInRoom = maxY $ S.toList ctx.room
      start = fmap (+ V2 2 (highestInRoom + 4)) rock
      Left (final, gusts') = foldM go (start, ctx.gusts) $ [0 ..]
      room' = S.union ctx.room . S.fromList $ final
      highestTracked = max (maxY final) highestInRoom
      highest' = ctx.highest + highestTracked - highestInRoom
   in Ctx{gusts = gusts', highest = highest', room = cleanupRoom nHistory room'}
 where
  go :: (Shape, [(Int, Gust)]) -> Int -> Either (Shape, [(Int, Gust)]) (Shape, [(Int, Gust)])
  go (s, gs) _ =
    let pushAttempt = fmap (+ toDir (snd $ head gs)) s
        pushed = if collides ctx.room pushAttempt then s else pushAttempt
        dropAttempt = fmap (+ V2 0 (-1)) pushed
     in if collides ctx.room dropAttempt
          then Left (pushed, tail gs)
          else Right (dropAttempt, tail gs)

  collides :: Set Point -> Shape -> Bool
  collides r =
    any (\p@(V2 x y) -> p `S.member` r || y <= 0 || x < 0 || x > 6)

solveA :: [Gust] -> Int
solveA gusts =
  (.highest)
    . (!! 2022)
    . scanl' (fall 100) Ctx{gusts = zip [0 ..] (cycle gusts), highest = 0, room = S.empty}
    $ cycle rocks

day17a :: Solution [Gust] Int
day17a =
  Solution
    { sParse = traverse (readEither . (: []))
    , sShow = show
    , sSolve = Right . solveA
    }

-- Finds the first place `xs` repeats and returns both indices.
findCycle :: (Ord a) => [a] -> Maybe (Int, Int)
findCycle xs =
  listToMaybe [(i, j) | (j, Just i) <- zip [0 ..] $ zipWith M.lookup xs xis]
 where
  -- Map of x to the index it's seen at, for each x.
  xis = scanl' (flip $ uncurry M.insert) M.empty $ zip xs [0 ..]

solveB :: Int -> [Gust] -> Int
solveB n gusts =
  let
    gustCycleLength = length gusts
    rockCycleLength = length rocks

    -- Find a cycle where:
    --   the same rock is being dropped
    --   the gusts are at the same place in the cycle
    --   the last 100 lines in the room look the same.
    states =
      scanl'
        (fall 100)
        Ctx{gusts = zip [0 ..] (cycle gusts), highest = 0, room = S.empty}
        $ cycle rocks

    cycleInfo =
      zipWith
        (\ri ctx -> (ri, fst (head ctx.gusts) `mod` gustCycleLength, ctx.room))
        (cycle [1 .. rockCycleLength])
        states

    c = findCycle (take n cycleInfo)

    (cStart, cFin) = case c of
      Just (x, y) -> (x, y)
      Nothing -> error "No cycle detected"

    -- Now that a cycle has been found, repeat the cycle until the target
    -- is reached - but stop short and then look up the final remainder
    -- of the moves.
    hStart = (.highest) $ states !! cStart
    hFin = (.highest) $ states !! cFin
    cLen = cFin - cStart
    (nCycles, r) = (n - cStart) `divMod` cLen
    remainderHeight = (states !! (cStart + r)).highest - hStart
   in
    remainderHeight + nCycles * (hFin - hStart) + hStart

day17b :: Solution [Gust] Int
day17b =
  Solution
    { sParse = traverse (readEither . (: []))
    , sShow = show
    , sSolve = Right . solveB 1000000000000
    }
