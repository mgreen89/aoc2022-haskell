{-# OPTIONS_GHC -Wno-unused-imports #-}

module AoC.Challenge (
  module AoC,
  ChallengeSpec (..),
  SomeSolution (..),
  SolutionError (..),
  ChallengeMap,
  challengeMap,
  getDay,
  getPart,
  ChallengePaths (..),
  challengePaths,
  ChallengeData (..),
  challengeData,
  TestData (..),
) where

import Advent.Extra
import AoC.Challenge.Day01 as AoC
import AoC.Challenge.Day02 as AoC
import AoC.Challenge.Day03 as AoC
import AoC.Challenge.Day04 as AoC
import AoC.Challenge.Day05 as AoC
import AoC.Challenge.Day06 as AoC
import AoC.Challenge.Day07 as AoC
import AoC.Challenge.Day08 as AoC
import AoC.Challenge.Day09 as AoC
import AoC.Challenge.Day10 as AoC
import AoC.Challenge.Day11 as AoC
import AoC.Challenge.Day12 as AoC
import AoC.Challenge.Day13 as AoC
import AoC.Challenge.Day14 as AoC
import AoC.Challenge.Day15 as AoC
import AoC.Challenge.Day16 as AoC
import AoC.Challenge.Day17 as AoC
import AoC.Challenge.Day18 as AoC
import AoC.Challenge.Day19 as AoC
import AoC.Challenge.Day20 as AoC
import AoC.Challenge.Day21 as AoC
import AoC.Challenge.Day22 as AoC
import AoC.Challenge.Day23 as AoC
import AoC.Challenge.Day24 as AoC
import AoC.Challenge.Day25 as AoC
import AoC.Config
import AoC.Solution
import AoC.Util
import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Data.Bifunctor
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Void (Void)
import System.Directory
import System.FilePath
import System.IO.Error
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import Text.Printf

-- | Type alias for the challenge map.
type ChallengeMap = Map Day (Map Part SomeSolution)

-- | A map of all challenges.
challengeMap :: ChallengeMap
challengeMap =
  M.unionsWith M.union
    . map (uncurry M.singleton . second (uncurry M.singleton))
    $ solutionList

-- | List of all the completed solutions.
solutionList :: [(Day, (Part, SomeSolution))]
solutionList =
  [ (mkDay_ 1, (Part1, SomeSolution day01a))
  , (mkDay_ 1, (Part2, SomeSolution day01b))
  , (mkDay_ 2, (Part1, SomeSolution day02a))
  , (mkDay_ 2, (Part2, SomeSolution day02b))
  , (mkDay_ 3, (Part1, SomeSolution day03a))
  , (mkDay_ 3, (Part2, SomeSolution day03b))
  , (mkDay_ 4, (Part1, SomeSolution day04a))
  , (mkDay_ 4, (Part2, SomeSolution day04b))
  , (mkDay_ 5, (Part1, SomeSolution day05a))
  , (mkDay_ 5, (Part2, SomeSolution day05b))
  , (mkDay_ 6, (Part1, SomeSolution day06a))
  , (mkDay_ 6, (Part2, SomeSolution day06b))
  , (mkDay_ 7, (Part1, SomeSolution day07a))
  , (mkDay_ 7, (Part2, SomeSolution day07b))
  , (mkDay_ 8, (Part1, SomeSolution day08a))
  , (mkDay_ 8, (Part2, SomeSolution day08b))
  , (mkDay_ 9, (Part1, SomeSolution day09a))
  -- , (mkDay_ 9 , (Part2, SomeSolution day09b))
  -- , (mkDay_ 10, (Part1, SomeSolution day10a))
  -- , (mkDay_ 10, (Part2, SomeSolution day10b))
  -- , (mkDay_ 11, (Part1, SomeSolution day11a))
  -- , (mkDay_ 11, (Part2, SomeSolution day11b))
  -- , (mkDay_ 12, (Part1, SomeSolution day12a))
  -- , (mkDay_ 12, (Part2, SomeSolution day12b))
  -- , (mkDay_ 13, (Part1, SomeSolution day13a))
  -- , (mkDay_ 13, (Part2, SomeSolution day13b))
  -- , (mkDay_ 14, (Part1, SomeSolution day14a))
  -- , (mkDay_ 14, (Part2, SomeSolution day14b))
  -- , (mkDay_ 15, (Part1, SomeSolution day15a))
  -- , (mkDay_ 15, (Part2, SomeSolution day15b))
  -- , (mkDay_ 16, (Part1, SomeSolution day16a))
  -- , (mkDay_ 16, (Part2, SomeSolution day16b))
  -- , (mkDay_ 17, (Part1, SomeSolution day17a))
  -- , (mkDay_ 17, (Part2, SomeSolution day17b))
  -- , (mkDay_ 18, (Part1, SomeSolution day18a))
  -- , (mkDay_ 18, (Part2, SomeSolution day18b))
  -- , (mkDay_ 19, (Part1, SomeSolution day19a))
  -- , (mkDay_ 19, (Part2, SomeSolution day19b))
  -- , (mkDay_ 20, (Part1, SomeSolution day20a))
  -- , (mkDay_ 20, (Part2, SomeSolution day20b))
  -- , (mkDay_ 21, (Part1, SomeSolution day21a))
  -- , (mkDay_ 21, (Part2, SomeSolution day21b))
  -- , (mkDay_ 22, (Part1, SomeSolution day22a))
  -- , (mkDay_ 22, (Part2, SomeSolution day22b))
  -- , (mkDay_ 23, (Part1, SomeSolution day23a))
  -- , (mkDay_ 23, (Part2, SomeSolution day23b))
  -- , (mkDay_ 25, (Part1, SomeSolution day25a))
  ]

-- | Get a map of the completed solution parts for the given day.
getDay :: ChallengeMap -> Day -> Either String (Map Part SomeSolution)
getDay cm d =
  maybeToEither (printf "Day not yet available: %d" (dayInt d)) $ M.lookup d cm

-- | Get the solution to the given part.
getPart :: Map Part SomeSolution -> Part -> Either String SomeSolution
getPart ps p =
  maybeToEither (printf "Part not found: %c" (partChar p)) $ M.lookup p ps

-- | Specification for a single challenge.
data ChallengeSpec = ChallengeSpec
  { day :: Day
  , part :: Part
  }
  deriving (Show)

-- | All the file paths associated with a single challenge.
data ChallengePaths = ChallengePaths
  { input :: !FilePath
  , tests :: !FilePath
  }
  deriving (Show)

-- | Get the challenge paths for the given challenge.
challengePaths :: ChallengeSpec -> ChallengePaths
challengePaths (ChallengeSpec d p) =
  ChallengePaths
    { input = "data" </> "input" </> printf "day%02d" d' <.> "txt"
    , tests = "data" </> "test" </> printf "day%02d%c" d' p' <.> "txt"
    }
 where
  d' = dayInt d
  p' = partChar p

makeChallengePathDirs :: ChallengePaths -> IO ()
makeChallengePathDirs cp =
  traverse_ (createDirectoryIfMissing True . takeDirectory) [cp.input, cp.tests]

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe fp = do
  readResult <- tryJust (guard . isDoesNotExistError) (readFile fp)
  traverse (evaluate . force) . eitherToMaybe $ readResult

-- | The associated input and test data for a given challenge.
data ChallengeData = ChallengeData
  { input :: !(Either [String] String)
  , tests :: ![TestData]
  }

{- | Get the associated data for a given challenge.
 This will fetch input from online (if not already available) if a session
 token is present in the given configuration.
-}
challengeData :: Config -> ChallengeSpec -> IO ChallengeData
challengeData cfg spec = do
  makeChallengePathDirs cps
  inp <-
    runExceptT
      . asum
      $ [ maybeToEither [printf "Input file not found at %s" cps.input]
            =<< liftIO (readFileMaybe cps.input)
        , fetchInput
        ]
  ts <-
    readFileMaybe cps.tests >>= \case
      Nothing -> pure []
      Just s -> case MP.parse parseTests cps.tests s of
        -- Put [] in the IO functor (no test data), and print an error.
        Left e -> [] <$ putStrLn (MP.errorBundlePretty e)
        Right r -> pure r

  return ChallengeData{input = inp, tests = ts}
 where
  cps = challengePaths spec
  fetchInput :: ExceptT [String] IO String
  fetchInput = do
    sessKey <- maybeToEither ["Session key needed to fetch input"] cfg.session
    let opts = defaultAoCOpts cfg.year sessKey
    inp <- liftEither . bimap showAoCError T.unpack =<< liftIO (runAoC opts a)
    liftIO $ writeFile cps.input inp
    pure inp
   where
    a = AoCInput spec.day

-- | Input and expected answer for a single test.
data TestData = TestData
  { input :: String
  , answer :: String
  }

parseTests :: MP.Parsec Void String [TestData]
parseTests = MP.many parseTest <* MP.eof

parseTest :: MP.Parsec Void String TestData
parseTest = do
  inp <- MP.manyTill MP.anySingle $ MP.lookAhead (MP.string ">>>")
  ans <-
    MP.string ">>>"
      *> MP.space1
      *> MP.many (MP.anySingleBut '\n')
      <* (MP.single '\n' <|> ('\n' <$ MP.lookAhead MP.eof))
  pure TestData{input = inp, answer = ans}
