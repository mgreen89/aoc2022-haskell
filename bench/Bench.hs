import AoC
import Control.Monad.Except

main :: IO ()
main = do
  cfg <- readConfig defConfPath
  void . runExceptT . mainRun cfg $ (defaultRunOpts TSAll){_roBench = True}
