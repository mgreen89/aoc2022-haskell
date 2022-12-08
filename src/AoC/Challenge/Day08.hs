module AoC.Challenge.Day08 (
  day08a,
  day08b,
)
where

import AoC.Solution
import Data.Char (digitToInt)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (findIndex, transpose)
import Data.Maybe (isJust)
import Linear (V2 (..), V4 (..))

type Point = V2 Int

parse :: String -> [[Int]]
parse = fmap (fmap digitToInt) . lines

sections :: [[Int]] -> (IntMap [Int], IntMap [Int])
sections inp =
  (IM.fromList $ zip [0 ..] inp, IM.fromList $ zip [0 ..] (transpose inp))

getViews :: (IntMap [Int], IntMap [Int]) -> Point -> (Int, V4 [Int])
getViews (horiz, vert) (V2 x y) =
  (curr, V4 toTop toRight toBottom toLeft)
 where
  (fromLeft, currToRight) = splitAt x (horiz IM.! y)
  (fromTop, currToBottom) = splitAt y (vert IM.! x)

  toLeft = reverse fromLeft
  curr = head currToRight
  toRight = tail currToRight
  toTop = reverse fromTop
  toBottom = tail currToBottom

isVisible :: (IntMap [Int], IntMap [Int]) -> Point -> Bool
isVisible ss p =
  any (isJust . findIndex (< pos)) views
 where
  (pos, views) = getViews ss p

loopMap :: ((IntMap [Int], IntMap [Int]) -> Point -> a) -> [[Int]] -> [a]
loopMap f inp =
  let ss@(horiz, vert) = sections inp
   in [f ss (V2 x y) | y <- IM.keys horiz, x <- IM.keys vert]

day08a :: Solution [[Int]] Int
day08a =
  Solution
    { sParse = Right . parse
    , sShow = show
    , sSolve = Right . length . filter id . loopMap isVisible
    }

scenicScore :: (IntMap [Int], IntMap [Int]) -> Point -> Int
scenicScore ss p =
  product $ countSeen pos <$> views
 where
  (pos, views) = getViews ss p
  countSeen :: Int -> [Int] -> Int
  countSeen p ts =
    case findIndex (>= p) ts of
      Just i -> i + 1
      Nothing -> length ts

day08b :: Solution [[Int]] Int
day08b =
  Solution
    { sParse = Right . parse
    , sShow = show
    , sSolve = Right . maximum . loopMap scenicScore
    }
