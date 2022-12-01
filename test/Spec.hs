import AoC
import Control.Monad.Except
import qualified System.Console.ANSI as ANSI
import System.Exit

main :: IO ()
main = do
  putStrLn ""
  cfg <- readConfig defConfPath
  out <- runExceptT $ mainRun cfg $ (defaultRunOpts RSAll){_roTest = True}
  case out of
    Left e -> do
      withColor ANSI.Vivid ANSI.Red $ putStrLn "[ERROR]"
      mapM_ putStrLn e
      exitFailure
    Right _ -> pure ()
