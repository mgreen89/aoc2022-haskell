{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day22 (
  day22a,
  day22b,
) where

import AoC.Common (listTup2)
import AoC.Solution
import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, mapMaybe)
import Data.Void (Void)
import GHC.Generics (Generic)
import Linear (V2 (..))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

type Point = V2 Int
data Tile = Open | Wall deriving (Generic, NFData)

instance Show Tile where
  show Open = "."
  show Wall = "#"

parseMap :: String -> Map Point Tile
parseMap =
  M.fromList
    . mapMaybe (traverse toTile)
    . concat
    . zipWith (\y -> zipWith (\x -> (V2 x y,)) [1 ..]) [1 ..]
    . lines
 where
  toTile ' ' = Nothing
  toTile '.' = Just Open
  toTile '#' = Just Wall
  toTile t = error $ "Invalid tile: " ++ [t]

data Dir = U | R | D | L deriving (Show, Generic, NFData)

parseDir :: Char -> Either String Dir
parseDir 'U' = Right U
parseDir 'R' = Right R
parseDir 'D' = Right D
parseDir 'L' = Right L
parseDir d = Left $ "Invalid dir: " ++ [d]

dirParser :: MP.Parsec Void String Dir
dirParser =
  MP.choice
    [ U <$ MP.single 'U'
    , R <$ MP.single 'R'
    , D <$ MP.single 'D'
    , L <$ MP.single 'L'
    ]

parseDir' :: Char -> Dir
parseDir' c = case parseDir c of
  Left e -> error e
  Right d -> d

dirPoint :: Dir -> Point
dirPoint = \case
  U -> V2 0 (-1)
  R -> V2 1 0
  D -> V2 0 1
  L -> V2 (-1) 0

dirRot :: Dir -> Dir -> Dir
dirRot U = id
dirRot R = \case
  U -> R
  R -> D
  D -> L
  L -> U
dirRot D = \case
  U -> D
  R -> L
  D -> U
  L -> R
dirRot L = \case
  U -> L
  R -> U
  D -> R
  L -> D

rot :: Dir -> V2 Int -> V2 Int
rot = \case
  U -> id
  R -> \(V2 x y) -> V2 y (-x)
  D -> negate
  L -> \(V2 x y) -> V2 (-y) x

data Instr
  = Move Int
  | Turn Dir
  deriving (Show, Generic, NFData)

instrParser :: MP.Parsec Void String Instr
instrParser =
  MP.choice
    [ Move <$> MPL.decimal
    , Turn <$> dirParser
    ]

parse :: String -> Either String (Map Point Tile, [Instr])
parse s =
  let (mapString, instrString) = fromJust . listTup2 . splitOn "\n\n" $ s
   in fmap (parseMap mapString,)
        . first MP.errorBundlePretty
        . MP.parse (MP.some instrParser) "day22"
        $ instrString

getFirstInRow :: Map Point Tile -> Int -> (Point, Tile)
getFirstInRow tiles y =
  head . M.toAscList . M.filterWithKey (\(V2 _ py) _ -> y == py) $ tiles

getNextOpenWrap :: Map Point Tile -> Point -> Dir -> Maybe Point
getNextOpenWrap tiles pos@(V2 px py) dir =
  let
    simpleNext = pos + dirPoint dir
   in
    case tiles M.!? simpleNext of
      Just Open -> Just simpleNext
      Just Wall -> Nothing
      Nothing ->
        let wrappedNext = case dir of
              U -> bottomCol
              R -> beginRow
              D -> topCol
              L -> endRow
         in case snd wrappedNext of
              Open -> Just $ fst wrappedNext
              Wall -> Nothing
 where
  topCol = head . M.toAscList . M.filterWithKey (\(V2 x _) _ -> x == px) $ tiles
  bottomCol = head . M.toDescList . M.filterWithKey (\(V2 x _) _ -> x == px) $ tiles
  beginRow = head . M.toAscList . M.filterWithKey (\(V2 _ y) _ -> y == py) $ tiles
  endRow = head . M.toDescList . M.filterWithKey (\(V2 _ y) _ -> y == py) $ tiles

solveA :: (Map Point Tile, [Instr]) -> Int
solveA (tiles, instrs) =
  let
    start = (fst $ getFirstInRow tiles 1, R)
   in
    getScore $ foldl' go start instrs
 where
  go :: (Point, Dir) -> Instr -> (Point, Dir)
  go (p, d) = \case
    Move n -> (move n d p, d)
    Turn t -> (p, dirRot t d)

  move :: Int -> Dir -> Point -> Point
  move 0 _ p = p
  move n d p = case getNextOpenWrap tiles p d of
    Just next -> move (n - 1) d next
    Nothing -> p

  getScore :: (Point, Dir) -> Int
  getScore (V2 x y, d) =
    1000 * y
      + 4 * x
      + ( case d of
            U -> 3
            R -> 0
            D -> 1
            L -> 2
        )

day22a :: Solution (Map Point Tile, [Instr]) Int
day22a = Solution{sParse = parse, sShow = show, sSolve = Right . solveA}

day22b :: Solution _ _
day22b = Solution{sParse = Right, sShow = show, sSolve = Right}
