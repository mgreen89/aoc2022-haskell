module AoC.Challenge.Day21 (
  day21a,
  day21b,
)
where

import AoC.Solution
import Control.Applicative (liftA2)
import Data.Bifunctor (first)
import Data.Map (Map)
import qualified Data.Map as M

toOp :: Integral a => String -> a -> a -> a
toOp "+" = (+)
toOp "-" = (-)
toOp "/" = div
toOp "*" = (*)
toOp x = error $ "Invalid op: " ++ x

parse :: Map String Int -> [String] -> (String, Int)
parse _ [m, v] = (init m, read v)
parse ms [m, a, o, b] = (init m, toOp o (ms M.! a) (ms M.! b))
parse _ x = error $ "Invalid parse: " ++ unwords x

solveA :: [String] -> Int
solveA inp = ms M.! "root"
 where
  -- Recursive definition - just let Haskell sort it out.
  ms = M.fromList . fmap (parse ms . words) $ inp

day21a :: Solution [String] Int
day21a =
  Solution
    { sParse = Right . lines
    , sShow = show
    , sSolve = Right . solveA
    }

data P = Tgt | Op String (Either String Int) (Either String Int) deriving (Show)

parseB :: Map String (Either P Int) -> [String] -> (String, Either P Int)
parseB _ ["humn:", _] = ("humn", Left Tgt)
parseB _ [m, v] = (init m, Right $ read v)
parseB ms [m, a, o, b] =
  ( init m
  , first
      (const $ Op o (first (const a) $ ms M.! a) (first (const b) $ ms M.! b))
      $ liftA2 (toOp o) (ms M.! a) (ms M.! b)
  )
parseB _ x = error $ "Invalid parse: " ++ unwords x

solveB :: [String] -> Int
solveB inp = go tgt next
 where
  go :: Int -> String -> Int
  go !t n =
    case ms M.! n of
      Left (Op "-" (Left x) (Right v)) -> go (v + t) x
      Left (Op "-" (Right v) (Left y)) -> go (v - t) y
      Left (Op "+" (Left x) (Right v)) -> go (t - v) x
      Left (Op "+" (Right v) (Left y)) -> go (t - v) y
      Left (Op "/" (Left x) (Right v)) -> go (v * t) x
      Left (Op "/" (Right v) (Left y)) -> go (v `div` t) y
      Left (Op "*" (Left x) (Right v)) -> go (t `div` v) x
      Left (Op "*" (Right v) (Left y)) -> go (t `div` v) y
      Left (Op _ _ _) -> error "invalid node in backtracking"
      Left Tgt -> t
      _ -> error "invalid node in backtracking"

  ms = M.fromList . fmap (parseB ms . words) $ inp
  root = ms M.! "root"
  (tgt, next) = case root of
    Left (Op _ (Left x) (Right v)) -> (v, x)
    Left (Op _ (Right v) (Left y)) -> (v, y)
    _ -> error "Invalid root"

day21b :: Solution [String] Int
day21b =
  Solution
    { sParse = Right . lines
    , sShow = show
    , sSolve = Right . solveB
    }
