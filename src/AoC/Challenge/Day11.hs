{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day11 (
  day11a,
)
where

-- , day11b

import AoC.Solution
import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (sort)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Byte as MPL
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

data Monkey = Monkey
  { items :: Seq Int
  , op :: Int -> Int
  , test :: Int -> Bool
  , trueTo :: Int
  , falseTo :: Int
  }
  deriving (Generic, NFData)

{- What a parse, lol.
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3
-}
monkeyParser :: MP.Parsec Void String (Int, Monkey)
monkeyParser = do
  MP.string "Monkey "
  i <- MPL.decimal
  MP.string ":"
  MP.space <* MP.string "Starting items: "
  items <-
    S.fromList
      <$> MP.some
        ( MPL.decimal
            <* MP.optional (MP.string "," <* MP.space)
        )
  MP.space <* MP.string "Operation: new = old "
  operator <- MP.choice [(+) <$ MP.char '+', (*) <$ MP.char '*'] <* MP.space
  op <-
    MP.choice
      [ (\x -> operator x x) <$ MP.string "old"
      , operator <$> MPL.decimal
      ]
  MP.space <* MP.string "Test: divisible by "
  test <- (\d v -> v `mod` d == 0) <$> MPL.decimal
  MP.space <* MP.string "If true: throw to monkey "
  trueTo <- MPL.decimal
  MP.space <* MP.string "If false: throw to monkey "
  falseTo <- MPL.decimal
  MP.space

  pure (i, Monkey{..})

parseMonkeys :: String -> Either String [(Int, Monkey)]
parseMonkeys =
  first MP.errorBundlePretty . MP.parse (MP.some monkeyParser) "day11"

monkeyTurn :: IntMap (Monkey, Int) -> Int -> IntMap (Monkey, Int)
monkeyTurn ms i =
  IM.union (IM.singleton i (m', c')) $ foldl' go ms m.items
 where
  (m, c) = ms IM.! i
  c' = c + S.length m.items
  m' = m{items = S.empty}

  go :: IntMap (Monkey, Int) -> Int -> IntMap (Monkey, Int)
  go mms w =
    let w' = m.op w `div` 3
        tgt = if m.test w' then m.trueTo else m.falseTo
        (tm, tmc) = mms IM.! tgt
        tm' = IM.singleton tgt (tm{items = tm.items S.|> w'}, tmc)
     in IM.union tm' mms

monkeyRound :: IntMap (Monkey, Int) -> IntMap (Monkey, Int)
monkeyRound ms =
  foldl' monkeyTurn ms (IM.keys ms)

solveA :: [(Int, Monkey)] -> Int
solveA inp =
  let
    ms :: IntMap (Monkey, Int)
    ms = IM.fromList . fmap (fmap (, 0)) $ inp

    end = iterate monkeyRound ms !! 20
  in
  product . take 2 . reverse . sort . fmap (snd . snd) . IM.toList $ end

day11a :: Solution [(Int, Monkey)] Int
day11a =
  Solution
    { sParse = parseMonkeys
    , sShow = show
    , sSolve = Right . solveA
    }

day11b :: Solution _ _
day11b = Solution{sParse = Right, sShow = show, sSolve = Right}
