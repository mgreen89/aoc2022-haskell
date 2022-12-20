module AoC.Run (
  RunSpec (..),
  RunOpts (..),
  defaultRunOpts,
  mainRun,
  SubmitOpts (..),
  defaultSubmitOpts,
  mainSubmit,
) where

import Advent.Extra
import AoC.Challenge
import AoC.Config
import AoC.Solution
import AoC.Util
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Criterion
import Data.Bifunctor
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified System.Console.ANSI as ANSI
import Text.Printf

-- | Specification for which solutions to run (and optionally test/bench etc...)
data RunSpec
  = RSAll
  | RSDayAll {day :: Day}
  | RSDayPart {spec :: ChallengeSpec}
  deriving (Show)

-- | Options for "run" mode.
data RunOpts = RunOpts
  { spec :: !RunSpec
  , actual :: !Bool
  , test :: !Bool
  , bench :: !Bool
  }
  deriving (Show)

-- | Default "run" mode options.
defaultRunOpts :: RunSpec -> RunOpts
defaultRunOpts rs =
  RunOpts{spec = rs, actual = True, test = False, bench = False}

-- | Options for "submit" mode.
data SubmitOpts = SubmitOpts
  { spec :: !ChallengeSpec
  , test :: !Bool
  , _force :: !Bool
  }
  deriving (Show)

-- | Default "submit" mode options.
defaultSubmitOpts :: ChallengeSpec -> SubmitOpts
defaultSubmitOpts cs =
  SubmitOpts{spec = cs, test = True, _force = False}

-- | Main function for the "run" mode.
mainRun ::
  (MonadIO m, MonadError [String] m) =>
  Config ->
  RunOpts ->
  m (Map Day (Map Part (Either [String] String)))
mainRun cfg ro = do
  solsToRun <- liftEither . first (: []) . filterChallengeMap $ ro.spec
  liftIO $ flip M.traverseWithKey solsToRun $ \d ->
    M.traverseWithKey $ \p -> runOne cfg ro d p

-- | Main function for the "submit" mode.
mainSubmit ::
  (MonadIO m, MonadError [String] m) =>
  Config ->
  SubmitOpts ->
  m (Text, SubmitRes)
mainSubmit cfg so = do
  sessKey <- maybeToEither ["ERROR: Session Key required to submit"] cfg.session
  cd <- liftIO $ challengeData cfg so.spec
  dps <- liftEither . first (: []) . getDay challengeMap $ so.spec.day
  sol <- liftEither . first (: []) . getPart dps $ so.spec.part
  inp <- liftEither $ first ("ERROR: No input" :) cd.input

  when so.test $ do
    testRes <- liftIO $ runTestSuite sol cd
    unless (and testRes) $
      if so._force
        then do
          liftIO $
            withColor ANSI.Vivid ANSI.Red $
              putStrLn
                "Forcing submission with test errors!"
        else throwError ["Submission aborted."]
  res <-
    liftEither
      . first (("Solution Error: " :) . (: []) . showSolutionError)
      $ runSomeSolution sol inp
  liftIO $ printf "Submitting solution %s\n" res

  let opts = defaultAoCOpts cfg.year sessKey
      submit = runAoC opts (AoCSubmit so.spec.day so.spec.part res)
  output@(_, status) <- liftEither . first showAoCError =<< liftIO submit

  let (color, out) = displayStatus status
  liftIO $ withColor ANSI.Vivid color $ putStrLn out
  pure output
 where
  displayStatus :: SubmitRes -> (ANSI.Color, String)
  displayStatus sr =
    let color = case sr of
          SubCorrect _ -> ANSI.Green
          SubIncorrect _ _ -> ANSI.Red
          SubWait _ -> ANSI.Yellow
          SubInvalid -> ANSI.Blue
          SubUnknown _ -> ANSI.Magenta
     in (color, showAoCSubmitRes sr)

runOne ::
  Config ->
  RunOpts ->
  Day ->
  Part ->
  SomeSolution ->
  IO (Either [String] String)
runOne cfg ro d p sol = do
  withColor ANSI.Dull ANSI.Blue $ printf ">> Day %02d%c" (dayInt d) (partChar p)
  cd <- challengeData cfg (ChallengeSpec d p)
  if ro.test
    then do
      printf "\n"
      runTestSuite sol cd
      printf "Answer :"
    else printf " "
  case cd.input of
    Right inp
      | ro.bench -> do
          _ <- evaluate (force inp)
          case sol of
            SomeSolution s -> do
              -- Run the solution to check it succeeds.
              case runSolution s inp of
                Left e ->
                  -- Failure, don't benchmark, just print error.
                  withColor ANSI.Vivid ANSI.Red $
                    putStrLn $
                      "Skipping benchmark - failure: " ++ showSolutionError e
                Right _ ->
                  -- Success, run the benchmark.
                  benchmark $ nf (s.sSolve <=< s.sParse) (stripNewlines inp)
              pure $ Left ["No results when benchmarking"]
      | ro.actual -> first ((: []) . show) <$> runSol sol inp
      | otherwise -> pure $ Left ["Skipping!"]
    Left e
      | ro.test ->
          pure (Left ["Ran tests and no main input"])
      | otherwise ->
          Left e <$ putStrLn "[INPUT ERROR]" <* traverse_ putStrLn e

runTestSuite :: SomeSolution -> ChallengeData -> IO (Maybe Bool)
runTestSuite sol cd = do
  res <- traverse (runTestCase sol) cd.tests
  unless (null res) $ do
    let (mark, color) = if and res then ('✓', ANSI.Green) else ('✗', ANSI.Red)
    withColor ANSI.Vivid color $
      printf "[%c] %d/%d passed\n" mark (length (filter id res)) (length res)
  pure $ and res <$ guard (not (null res))

type SolutionResult = Either SolutionError String

solResString :: SolutionResult -> String
solResString res = case res of
  Right r -> r
  Left e -> printf "[ERROR] %s" (showSolutionError e)

runTestCase :: SomeSolution -> TestData -> IO Bool
runTestCase sol td = do
  withColor ANSI.Dull color $ printf "[%c]" mark
  printf " (%s)" (solResString res)
  if pass
    then printf "\n"
    else withColor ANSI.Vivid ANSI.Red $ printf " (Expected: %s)\n" td.answer
  return pass
 where
  res = runSomeSolution sol td.input
  (mark, pass, color) = case res of
    Right r ->
      if strip td.answer == strip r
        then ('✓', True, ANSI.Green)
        else ('✗', False, ANSI.Red)
    Left _ -> ('✗', False, ANSI.Red)

runSol :: SomeSolution -> String -> IO SolutionResult
runSol sol inp = do
  printf " %s\n" (solResString res)
  return res
 where
  res = runSomeSolution sol inp

filterChallengeMap :: RunSpec -> Either String ChallengeMap
filterChallengeMap = \case
  RSAll -> pure challengeMap
  RSDayAll d -> do
    ps <- getDay challengeMap d
    pure $ M.singleton d ps
  RSDayPart (ChallengeSpec d p) -> do
    ps <- getDay challengeMap d
    c <- getPart ps p
    pure $ M.singleton d (M.singleton p c)
