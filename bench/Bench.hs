import Advent
import AoC
import Control.DeepSeq (force)
import Control.Monad ((<=<))
import qualified Criterion as C
import qualified Criterion.Main as C
import qualified Criterion.Types as C
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import GHC.IO (evaluate)
import Text.Printf (printf)

getNf :: Config -> Day -> Part -> SomeSolution -> IO (Maybe C.Benchmark)
getNf cfg d p sol = do
  cd <- challengeData cfg (ChallengeSpec d p)
  let name = printf "Day %02d%c" (dayInt d) (partChar p)
  case cd.input of
    Right inp -> do
      _ <- evaluate (force inp)
      case sol of
        SomeSolution s -> do
          -- Run the solution to check it succeeds.
          case runSolution s inp of
            Left _ -> pure Nothing
            Right _ ->
              -- Success, get the benchmark.
              pure $ Just (C.bench name $ C.nf (s.sSolve <=< s.sParse) (stripNewlines inp))
    _ -> pure Nothing

main :: IO ()
main = do
  cfg <- readConfig defConfPath
  benches <-
    sequenceA $
      M.foldrWithKey
        ( \d ps l ->
            M.foldrWithKey
              ( \p s l' -> getNf cfg d p s : l'
              )
              l
              ps
        )
        []
        challengeMap
  C.defaultMainWith
    C.defaultConfig{C.reportFile = Just "bench.html"}
    (catMaybes benches)
