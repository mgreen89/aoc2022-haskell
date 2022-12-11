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
import Data.List (sortBy)
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

monkeyItem :: (Int -> Int) -> Int -> ItemFn
monkeyItem relief lcmWorry m ms w =
  let w' = relief (m.op w) `mod` lcmWorry
      tgt = if w' `mod` m.test == 0 then m.trueTo else m.falseTo
      (tm, tmc) = ms IM.! tgt
      tm' = (tm{items = tm.items S.|> w'}, tmc)
   in IM.insert tgt tm' ms

monkeyTurn :: ItemFn -> IntMap (Monkey, Int) -> Int -> IntMap (Monkey, Int)
monkeyTurn f ms i =
  IM.insert i (m', c') $ foldl' (f m) ms m.items
 where
  (m, c) = ms IM.! i
  m' = m{items = S.empty}
  c' = c + S.length m.items

monkeyRound :: ItemFn -> IntMap (Monkey, Int) -> IntMap (Monkey, Int)
monkeyRound f ms =
  foldl' (monkeyTurn f) ms (IM.keys ms)

solve :: Int -> ItemFn -> [(Int, Monkey)] -> Int
solve nRounds f =
    product
      . take 2
      . sortBy (flip compare)
      . fmap (snd . snd)
      . IM.toList
      . (!! nRounds)
      . iterate (monkeyRound f)
      . IM.fromList
      . fmap (fmap (, 0))

day11 :: Int -> (Int -> Int) -> Solution [(Int, Monkey)] Int
day11 nRounds relief =
  Solution
    { sParse = parseMonkeys
    , sShow = show
    , sSolve = \x ->
        let modAll = product . fmap ((.test) . snd) $ x
         in Right . solve nRounds (monkeyItem relief modAll) $ x
    }

day11a :: Solution [(Int, Monkey)] Int
day11a = day11 20 (`div` 3)

day11b :: Solution [(Int, Monkey)] Int
day11b = day11 10000 id
