module AoC.Challenge.Day19 (
  day19a,
  day19b,
)
where

import AoC.Solution
import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

data Resource = Ore | Clay | Obsidian deriving (Show, Generic, NFData)

type Recipe = [(Resource, Int)]

data Blueprint = Blueprint
  { ore :: Recipe
  , clay :: Recipe
  , obsidian :: Recipe
  , geode :: Recipe
  }
  deriving (Show, Generic, NFData)

blueprintParser :: MP.Parsec Void String (Int, Blueprint)
blueprintParser = do
  MP.string "Blueprint "
  i <- MPL.decimal <* MP.string ":" <* MP.space
  MP.string "Each ore robot costs "
  ore <- recipe <* MP.string "." <* MP.space
  MP.string "Each clay robot costs "
  clay <- recipe <* MP.string "." <* MP.space
  MP.string "Each obsidian robot costs "
  obsidian <- recipe <* MP.string "." <* MP.space
  MP.string "Each geode robot costs "
  geode <- recipe <* MP.string "."
  pure (i, Blueprint{..})
 where
  recipe :: MP.Parsec Void String Recipe
  recipe =
    flip MP.sepBy (MP.string " and ") $ do
      c <- MPL.decimal <* MP.space
      r <- resource
      pure (r, c)
  resource :: MP.Parsec Void String Resource
  resource =
    MP.choice
      [ Ore <$ MP.string "ore"
      , Clay <$ MP.string "clay"
      , Obsidian <$ MP.string "obsidian"
      ]

parse :: String -> Either String [(Int, Blueprint)]
parse =
  first MP.errorBundlePretty . MP.parse (MP.sepBy blueprintParser MP.space) "day19"

day19a :: Solution [(Int, Blueprint)] Int
day19a = Solution{sParse = parse, sShow = show, sSolve = Left . show}

day19b :: Solution [(Int, Blueprint)] Int
day19b = Solution{sParse = parse, sShow = show, sSolve = Left . const "not implemented"}
