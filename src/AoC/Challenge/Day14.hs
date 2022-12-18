module AoC.Challenge.Day14 (
  day14a,
  day14b,
)
where

import AoC.Common (pairs)
import AoC.Solution
import Data.Bifunctor (first)
import Data.List (unfoldr)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void (Void)
import Linear (V2 (..))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

type Point = V2 Int

lineParser :: MP.Parsec Void String [Point]
lineParser = do
  MP.sepBy
    (V2 <$> MPL.decimal <*> (MP.single ',' *> MPL.decimal))
    (MP.string " -> ")

parseLines :: String -> Either String [[Point]]
parseLines =
  traverse (first MP.errorBundlePretty . MP.parse lineParser "day14") . lines

pointsToSet :: [Point] -> Set Point
pointsToSet = S.fromList . concatMap (uncurry segment) . pairs
 where
  segment :: Point -> Point -> [Point]
  segment (V2 x1 y1) (V2 x2 y2)
    | x1 == x2 = [V2 x1 y | y <- [min y1 y2 .. max y1 y2]]
    | y1 == y2 = [V2 x y1 | x <- [min x1 x2 .. max x1 x2]]
    | otherwise = []

parseMap :: String -> Either String (Set Point)
parseMap = fmap (S.unions . fmap pointsToSet) . parseLines

fall :: Int -> Set Point -> Maybe (Either Int Point)
fall maxY slice
  | V2 500 0 `S.member` slice = Nothing
  | otherwise = Just $ fall' (V2 500 0)
 where
  fall' (V2 x y)
    | y > maxY = Left x
    | V2 x (y + 1) `S.notMember` slice = fall' (V2 x (y + 1))
    | V2 (x - 1) (y + 1) `S.notMember` slice = fall' (V2 (x - 1) (y + 1))
    | V2 (x + 1) (y + 1) `S.notMember` slice = fall' (V2 (x + 1) (y + 1))
    | otherwise = Right (V2 x y)

solveA :: Set Point -> Int
solveA slice =
  length $ unfoldr go slice
 where
  maxY :: Int
  maxY = maximum . fmap (\(V2 _ y) -> y) . S.toList $ slice
  go :: Set Point -> Maybe (Point, Set Point)
  go s =
    fall maxY s >>= either (const Nothing) (\p -> Just (p, p `S.insert` s))

day14a :: Solution (Set Point) Int
day14a = Solution{sParse = parseMap, sShow = show, sSolve = Right . solveA}

solveB :: Set Point -> Int
solveB slice =
  length $ unfoldr go slice
 where
  maxY :: Int
  maxY = (+ 1) . maximum . fmap (\(V2 _ y) -> y) . S.toList $ slice
  go :: Set Point -> Maybe ((), Set Point)
  go s =
    ((),) . (`S.insert` s) . either (`V2` maxY) id <$> fall maxY s

day14b :: Solution (Set Point) Int
day14b = Solution{sParse = parseMap, sShow = show, sSolve = Right . solveB}
