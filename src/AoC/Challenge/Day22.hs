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

data Dir = U | R | D | L deriving (Show, Eq, Ord, Enum, Generic, NFData)

dirParser :: MP.Parsec Void String Dir
dirParser =
  MP.choice
    [ U <$ MP.single 'U'
    , R <$ MP.single 'R'
    , D <$ MP.single 'D'
    , L <$ MP.single 'L'
    ]

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

-- Ugh.
-- Special case my solution
-- Faces like this:
--   A B
--   C
-- D E
-- F
-- xrange, yrange, top, right, bottom, left faces (and new direction)
faces :: [((Int, Int), (Int, Int), (Int, Dir), (Int, Dir), (Int, Dir), (Int, Dir))]
faces =
  [ ((51, 100), (1, 50), (5, R), (1, R), (2, D), (3, R))
  , ((101, 150), (1, 50), (5, U), (4, L), (2, L), (0, L))
  , ((51, 100), (51, 100), (0, U), (1, U), (4, D), (3, D))
  , ((1, 50), (101, 150), (2, R), (4, R), (5, D), (0, R))
  , ((51, 100), (101, 150), (2, U), (1, L), (5, L), (3, L))
  , ((1, 50), (151, 200), (3, U), (4, U), (1, D), (0, D))
  ]

{-
 U U x (-y)
 U R y x
 U D (-x) y
 U L (-y) (-x)

 R U y x
 R R (-x) y
 R D (-y) (-x)
 R L x (-y)
-}
rotRel :: Dir -> V2 Int -> V2 Int
rotRel = \case
  U -> \(V2 x y) -> V2 x (49 - y)
  R -> \(V2 x y) -> V2 y (x)
  D -> \(V2 x y) -> V2 (49 - x) (y)
  L -> \(V2 x y) -> V2 (49 - y) (49 - x)

-- Faces like this:
--     A
-- B C D
--     E F
{-
testFaces :: [((Int, Int), (Int, Int), (Int, Dir), (Int, Dir), (Int, Dir), (Int, Dir))]
testFaces =
  [ ((9, 12), (1, 4), (1, D), (5, L), (3, D), (2, D))
  , ((1, 4), (5, 8), (0, D), (2, R), (4, U), (3, U))
  , ((5, 8), (5, 8), (0, R), (3, R), (4, R), (1, L))
  , ((9, 12), (5, 8), (0, U), (5, D), (4, D), (2, L))
  , ((9, 12), (9, 12), (3, U), (5, R), (1, U), (2, U))
  , ((13, 16), (9, 12), (3, L), (0, L), (1, R), (4, L))
  ]

rotRelTest :: Dir -> V2 Int -> V2 Int
rotRelTest = \case
  U -> \(V2 x y) -> V2 x (3 - y)
  R -> \(V2 x y) -> V2 y (x)
  D -> \(V2 x y) -> V2 (3 - x) (y)
  L -> \(V2 x y) -> V2 (3 - y) (3 - x)
-}

overEdge :: (Point, Dir) -> (Point, Dir)
overEdge (V2 x y, d) =
  head $ mapMaybe go faces
 where
  go ((xLo, xHi), (yLo, yHi), t, r, b, l) =
    if xLo <= x && xHi >= x && yLo <= y && yHi >= y
      then
        let
          relX = x - xLo
          relY = y - yLo
          (nextFace, nextDir) = case d of
            U -> t
            R -> r
            D -> b
            L -> l
          ((nxLo, _), (nyLo, _), _, _, _, _) = faces !! nextFace
         in
          Just (rotRel (dirRot d nextDir) (V2 relX relY) + V2 nxLo nyLo, nextDir)
      else Nothing

getNextCube :: Map Point Tile -> Point -> Dir -> Maybe (Point, Dir)
getNextCube tiles pos dir =
  let
    simpleNext = pos + dirPoint dir
   in
    case tiles M.!? simpleNext of
      Just Open -> Just (simpleNext, dir)
      Just Wall -> Nothing
      Nothing ->
        -- Going over a cube edge.
        let (nextOverEdge, nextDir) = overEdge (pos, dir)
         in case tiles M.! nextOverEdge of
              Open -> Just (nextOverEdge, nextDir)
              Wall -> Nothing

solveB :: (Map Point Tile, [Instr]) -> Int
solveB (tiles, instrs) =
  let
    start = (fst $ getFirstInRow tiles 1, R)
   in
    getScore $ foldl' go start instrs
 where
  go :: (Point, Dir) -> Instr -> (Point, Dir)
  go (p, d) = \case
    Move n -> move n (p, d)
    Turn t -> (p, dirRot t d)

  move :: Int -> (Point, Dir) -> (Point, Dir)
  move 0 (p, d) = (p, d)
  move n (p, d) = case getNextCube tiles p d of
    Just (p', d') -> move (n - 1) (p', d')
    Nothing -> (p, d)

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

day22b :: Solution (Map Point Tile, [Instr]) Int
day22b = Solution{sParse = parse, sShow = show, sSolve = Right . solveB}
