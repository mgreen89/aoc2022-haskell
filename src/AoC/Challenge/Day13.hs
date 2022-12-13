module AoC.Challenge.Day13 (
  day13a,
  day13b,
)
where

import AoC.Solution
import AoC.Util (maybeToEither)
import Control.DeepSeq (NFData)
import Control.Monad (join)
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.List (intercalate, sort)
import Data.List.Split (splitOn)
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

data Packet
  = List [Packet]
  | Int Int
  deriving (Generic, NFData)

instance Show Packet where
  show (List l) = '[' : intercalate "," (fmap show l) ++ "]"
  show (Int i) = show i

instance Eq Packet where
  Int a == Int b = a == b
  x@(Int _) == y@(List _) = List [x] == y
  x@(List _) == y@(Int _) = x == List [y]
  (List a) == (List b) = a == b

instance Ord Packet where
  compare (Int a) (Int b) = compare a b
  compare x@(Int _) y@(List _) = compare (List [x]) y
  compare x@(List _) y@(Int _) = compare x (List [y])
  compare (List a) (List b) = compare a b

packetParser :: MP.Parsec Void String Packet
packetParser =
  MP.choice
    [ Int <$> MPL.decimal
    , List
        <$> MP.between
          (MP.string "[")
          (MP.string "]")
          (packetParser `MP.sepBy` MP.string ",")
    ]

listTup2 :: [a] -> Maybe (a, a)
listTup2 [x, y] = Just (x, y)
listTup2 _ = Nothing

parsePairs :: String -> Either String [(Packet, Packet)]
parsePairs =
  join
    . traverse (traverse (maybeToEither "Packets not in a pair" . listTup2))
    . first MP.errorBundlePretty
    . traverse (traverse (MP.parse packetParser "day13") . lines)
    . splitOn "\n\n"

day13a :: Solution [(Packet, Packet)] Int
day13a =
  Solution
    { sParse = parsePairs
    , sShow = show
    , sSolve =
        Right
          . sum
          . fmap fst
          . filter ((== LT) . snd)
          . zip [1 ..]
          . fmap (uncurry compare)
    }

parseAll :: String -> Either String [Packet]
parseAll =
  first MP.errorBundlePretty
    . traverse (MP.parse packetParser "day13")
    . filter (/= [])
    . lines

dividers :: [Packet]
dividers =
  [ List [List [Int 2]]
  , List [List [Int 6]]
  ]

findDividers :: [Packet] -> Int
findDividers =
  foldl' go 1 . zip [1 ..]
 where
  go :: Int -> (Int, Packet) -> Int
  go a (i, p) = if p `elem` dividers then a * i else a

day13b :: Solution [Packet] Int
day13b =
  Solution
    { sParse = parseAll
    , sShow = show
    , sSolve = Right . findDividers . sort . (dividers ++)
    }
