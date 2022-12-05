module AoC.Challenge.Day05 (
  day05a,
  day05b,
) where

import AoC.Solution
import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (transpose)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Void (Void)
import GHC.Generics (Generic)
import Safe (headMay)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

-- The top of the stack is the start of the string.
type Stack = String
data Move = Move
  { from :: !Int
  , to :: !Int
  , count :: !Int
  }
  deriving (Show, Generic, NFData)

type Stacks = IntMap Stack
type Input = (Stacks, [Move])

parser :: MP.Parsec Void String Input
parser = do
  lines <- MP.many stackParser
  MP.manyTill MP.anySingle (MP.string "\n\n")
  moves <- MP.many (moveParser <* MP.optional MP.newline)
  pure (linesToStacks lines, moves)
 where
  stackParser :: MP.Parsec Void String [Maybe Char]
  stackParser =
    MP.many
      ( MP.choice
          [ Just <$> (MP.char '[' *> MP.anySingle) <* MP.char ']'
          , Nothing <$ MP.string "   "
          ]
          <* MP.optional (MP.char ' ')
      )
      <* MP.newline

  moveParser :: MP.Parsec Void String Move
  moveParser = do
    -- Parse like 'move 1 from 2 to 1'
    MP.string "move "
    count <- MPL.decimal
    MP.string " from "
    from <- MPL.decimal
    MP.string " to "
    to <- MPL.decimal
    pure $ Move{..}

  linesToStacks :: [[Maybe Char]] -> Stacks
  linesToStacks =
    IM.fromList
      . zip [1 ..]
      . fmap (reverse . catMaybes)
      . transpose
      . reverse

parseInput :: String -> Either String Input
parseInput =
  first MP.errorBundlePretty . MP.parse parser "day05"

solve :: (Stacks -> Move -> Stacks) -> Input -> String
solve move (s, ms) =
  mapMaybe (headMay . snd) . IM.toList $ foldl' move s ms

day05 :: (Stacks -> Move -> Stacks) -> Solution Input String
day05 move =
  Solution
    { sParse = parseInput
    , sShow = id
    , sSolve = Right . solve move
    }

moveA :: Stacks -> Move -> Stacks
moveA s m
  | m.count == 0 = s
  | otherwise =
      let fromArr = s IM.! m.from
          m' = m{count = m.count - 1}
          s' = IM.adjust tail m.from . IM.adjust (head fromArr :) m.to $ s
       in moveA s' m'

day05a :: Solution Input String
day05a = day05 moveA

moveB :: Stacks -> Move -> Stacks
moveB s m =
  let
    fromArr = s IM.! m.from
   in
    IM.adjust (drop m.count) m.from
      . IM.adjust (take m.count fromArr ++) m.to
      $ s

day05b :: Solution Input String
day05b = day05 moveB
