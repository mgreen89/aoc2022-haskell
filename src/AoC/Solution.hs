module AoC.Solution (
  Solution (..),
  SomeSolution (..),
  SolutionError (..),
  showSolutionError,
  runSolution,
  runSomeSolution,
)
where

import AoC.Util
import Control.DeepSeq
import Data.Bifunctor
import GHC.Generics

{- | Abstraction over the type of a challenge solver to help with cleaner
 solutions.

 A @Solution a b@ encapsulates something that solves a challenge with input
 type @a@ into a response of type @b@.

 Consists of a parser, a solver, and a shower.  The solver solves
 a general @a -> 'Either' String b@ function, and the parser and shower
 are used to handle the boilerplate of parsing and printing the solution.
-}
data Solution a b = Solution
  { sParse :: String -> Either String a
  , sSolve :: a -> Either String b
  , sShow :: b -> String
  }

{- | Wrap a @Solution a b@ and hide the type variables so we can put
 different solutions in a container.
-}
data SomeSolution where
  -- NFData for two reasons:
  --  `a` so that the parse function can be forced, before...
  --  `b` so that the solve function can be benchmarked.
  SomeSolution :: (NFData a, NFData b) => Solution a b -> SomeSolution

-- | Errors that might happen when running a 'Solution' on some input.
data SolutionError
  = SEParse String
  | SESolve String
  deriving (Show, Generic, NFData)

showSolutionError :: SolutionError -> String
showSolutionError = \case
  SEParse s -> "Parse error: " <> s
  SESolve s -> "Solve error: " <> s

-- | Run a 'Solution' on some input.
runSolution :: Solution a b -> String -> Either SolutionError String
runSolution Solution{..} inp = do
  x <- first SEParse . sParse $ stripNewlines inp
  y <- first SESolve . sSolve $ x
  pure $ sShow y

-- | Run a 'SomeSolution' on some input.
runSomeSolution :: SomeSolution -> String -> Either SolutionError String
runSomeSolution (SomeSolution s) = runSolution s
