module AoC.Challenge.Day21 (
  day21a,
  day21b,
)
where

import AoC.Solution
import Data.Map (Map)
import qualified Data.Map as M

toOp :: String -> (Int -> Int -> Int)
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

day21b :: Solution [String] String
day21b =
  Solution
    { sParse = Right . lines
    , sShow = show
    , sSolve = Left . const "not implemented"
    }
