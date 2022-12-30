module AoC.Challenge.Day23 (
  day23a,
  day23b,
)
where

import AoC.Common.Point (Dir (..), boundingBox')
import AoC.Solution
import Data.Bits (shiftL, shiftR, (.&.))
import Data.Foldable (foldl')
import Data.Int (Int16)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Linear (V2 (..))

type Point = Int
type PointSet = IntSet

-- Works with coordinates in the range of a 16-bit signed integer,
-- i.e. in the range -32768 to 32767.

toPoint :: Int -> Int -> Point
toPoint x y = y `shiftL` 16 + x

toVec :: Point -> V2 Int
toVec p =
  let
    -- Cope with negative x.
    x :: Int16
    x = fromIntegral (p .&. 0xFFFF)
   in
    V2 (fromIntegral x) (p `shiftR` 16)

dirPoint :: Dir -> Point
dirPoint U = toPoint 0 (-1)
dirPoint R = toPoint 1 0
dirPoint D = toPoint 0 1
dirPoint L = toPoint (-1) 0

parseMap :: String -> PointSet
parseMap =
  IS.fromList
    . mapMaybe (\(p, c) -> if c == '#' then Just p else Nothing)
    . concat
    . zipWith (\y -> zipWith (\x -> (toPoint x y,)) [0 ..]) [0 ..]
    . lines

checks :: Dir -> [Point]
checks U = [toPoint (-1) (-1), toPoint 0 (-1), toPoint 1 (-1)]
checks R = [toPoint 1 (-1), toPoint 1 0, toPoint 1 1]
checks D = [toPoint (-1) 1, toPoint 0 1, toPoint 1 1]
checks L = [toPoint (-1) (-1), toPoint (-1) 0, toPoint (-1) 1]

allDiffs :: [Point]
allDiffs =
  [ toPoint 1 0
  , toPoint 1 1
  , toPoint 0 1
  , toPoint (-1) 1
  , toPoint (-1) 0
  , toPoint (-1) (-1)
  , toPoint 0 (-1)
  , toPoint 1 (-1)
  ]

propose :: Dir -> PointSet -> Point -> Point
propose start elves elf
  | all ((`IS.notMember` elves) . (+ elf)) allDiffs = elf
  | otherwise =
      fromMaybe elf
        . listToMaybe
        . mapMaybe go
        . take 4
        . iterate getNextDir
        $ start
 where
  go :: Dir -> Maybe Point
  go d =
    if all (`IS.notMember` elves) (fmap (+ elf) (checks d))
      then Just $ elf + dirPoint d
      else Nothing

doRound :: Dir -> PointSet -> PointSet
doRound firstDir elves =
  IS.foldl' go IS.empty elves
 where
  go a e =
    let e' = propose firstDir elves e
     in if e' `IS.member` a
          then -- Two moving to the same point.
          -- Max of two move from opposite directions, so restore the
          -- old point and add the current point.
            IS.delete e' . IS.insert (e' + e' - e) . IS.insert e $ a
          else IS.insert e' a

{-
Helpful debug output for the test inputs.
showSet :: Set Point -> String
showSet s =
  [ if x == 11 then '\n' else if p `S.member` s then '#' else '.'
  | y <- [-2 .. 9]
  , x <- [-3 .. 11]
  , let p = toPoint x y
  ]
-}

getNextDir :: Dir -> Dir
getNextDir U = D
getNextDir D = L
getNextDir L = R
getNextDir R = U

solveA :: PointSet -> Int
solveA start =
  bbSize - IS.size afterTen
 where
  rounds = fmap doRound . iterate getNextDir $ U
  afterTen = foldl' (\es rFn -> rFn es) start (take 10 rounds)
  afterTenVecs = IS.foldl' (\a p -> toVec p : a) [] afterTen
  (bbLo, bbHi) = case boundingBox' afterTenVecs of
    Just x -> x
    Nothing -> error "Elves disappeared!?"
  bbSize = product (bbHi - bbLo + V2 1 1)

day23a :: Solution PointSet Int
day23a =
  Solution
    { sParse = Right . parseMap
    , sShow = show
    , sSolve = Right . solveA
    }

solveB :: PointSet -> Int
solveB start =
  go start 1 U
 where
  go es i d =
    let es' = doRound d es
     in if es == es'
          then i
          else go es' (i + 1) (getNextDir d)

day23b :: Solution PointSet Int
day23b =
  Solution
    { sParse = Right . parseMap
    , sShow = show
    , sSolve = Right . solveB
    }
