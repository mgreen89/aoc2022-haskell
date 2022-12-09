module AoC.Challenge.Day09 (
  day09a,
  day09b,
)
where

import AoC.Solution
import Control.DeepSeq (NFData)
import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Linear (V2 (..))
import Text.Read (readEither)

type Point = V2 Int

type Rope = [Point]

data Dir = R | U | L | D deriving (Show, Generic, NFData)

data Move = Move {d :: Dir, l :: Int} deriving (Show, Generic, NFData)

parseMove :: String -> Either String Move
parseMove s = case words s of
  ["R", n] -> Move R <$> readEither n
  ["U", n] -> Move U <$> readEither n
  ["L", n] -> Move L <$> readEither n
  ["D", n] -> Move D <$> readEither n
  _ -> Left $ "Invalid line: " ++ s

dir :: Dir -> Point
dir = \case
  R -> V2 0 1
  U -> V2 (-1) 0
  L -> V2 0 (-1)
  D -> V2 1 0

moveRope :: (Rope, Set Point) -> Move -> (Rope, Set Point)
moveRope (r, ts) m
  | m.l == 0 = (r, ts)
  | otherwise =
      let (t, r') = moveOnce r m.d
       in moveRope (r', S.insert t ts) m{l = m.l - 1}
 where
  moveOnce :: Rope -> Dir -> (Point, Rope)
  moveOnce r d =
    let r' = foldl' moveKnot [head r + dir d] (tail r)
     in (head r', reverse r')

  moveKnot :: Rope -> Point -> Rope
  moveKnot r' p =
    let link = head r' - p
     in if all ((<= 1) . abs) link
          then p : r'
          else p + (min 1 . max (-1) <$> link) : r'

solve :: Int -> [Move] -> Int
solve ropeSize =
  S.size . snd . foldl' moveRope (replicate ropeSize (V2 0 0), S.singleton (V2 0 0))

day09a :: Solution [Move] Int
day09a =
  Solution
    { sParse = traverse parseMove . lines
    , sShow = show
    , sSolve = Right . solve 2
    }

day09b :: Solution [Move] Int
day09b =
  Solution
    { sParse = traverse parseMove . lines
    , sShow = show
    , sSolve = Right . solve 10
    }
