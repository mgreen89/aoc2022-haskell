module AoC.Challenge.Day11 (
  day11a,
  day11b,
)
where

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
  , test :: Int
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
  test <- MPL.decimal
  MP.space <* MP.string "If true: throw to monkey "
  trueTo <- MPL.decimal
  MP.space <* MP.string "If false: throw to monkey "
  falseTo <- MPL.decimal
  MP.space

  pure (i, Monkey{..})

parseMonkeys :: String -> Either String [(Int, Monkey)]
parseMonkeys =
  first MP.errorBundlePretty . MP.parse (MP.some monkeyParser) "day11"

type ItemFn = Monkey -> IntMap (Monkey, Int) -> Int -> IntMap (Monkey, Int)

monkeyTurn :: ItemFn -> IntMap (Monkey, Int) -> Int -> IntMap (Monkey, Int)
monkeyTurn f ms i =
  IM.union (IM.singleton i (m', c')) $ foldl' (f m) ms m.items
 where
  (m, c) = ms IM.! i
  c' = c + S.length m.items
  m' = m{items = S.empty}

monkeyRound :: ItemFn -> IntMap (Monkey, Int) -> IntMap (Monkey, Int)
monkeyRound f ms =
  foldl' (monkeyTurn f) ms (IM.keys ms)

solve :: Int -> ItemFn -> [(Int, Monkey)] -> Int
solve nRounds f inp =
  let
    ms :: IntMap (Monkey, Int)
    ms = IM.fromList . fmap (fmap (,0)) $ inp

    end = iterate (monkeyRound f) ms !! nRounds
   in
    product . take 2 . reverse . sort . fmap (snd . snd) . IM.toList $ end

monkeyItemA :: ItemFn
monkeyItemA m ms w =
  let w' = (m.op w `div` 3)
      tgt = if w' `mod` m.test == 0 then m.trueTo else m.falseTo
      (tm, tmc) = ms IM.! tgt
      tm' = IM.singleton tgt (tm{items = tm.items S.|> w'}, tmc)
   in IM.union tm' ms

day11a :: Solution [(Int, Monkey)] Int
day11a =
  Solution
    { sParse = parseMonkeys
    , sShow = show
    , sSolve = Right . solve 20 monkeyItemA
    }

monkeyItemB :: Int -> ItemFn
monkeyItemB i m ms w =
  let w' = m.op w `mod` i
      tgt = if w' `mod` m.test == 0 then m.trueTo else m.falseTo
      (tm, tmc) = ms IM.! tgt
      tm' = IM.singleton tgt (tm{items = tm.items S.|> w'}, tmc)
   in IM.union tm' ms

day11b :: Solution [(Int, Monkey)] Int
day11b =
  Solution
    { sParse = parseMonkeys
    , sShow = show
    , sSolve = \x ->
        let modAll = product . fmap ((.test) . snd) $ x
         in Right . solve 10000 (monkeyItemB modAll) $ x
    }
