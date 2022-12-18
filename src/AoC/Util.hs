module AoC.Util (
  freqs,
  strip,
  stripNewlines,
  eitherToMaybe,
  maybeToEither,
  withColor,
)
where

import Control.Applicative (Alternative, empty)
import Control.Monad.Except (MonadError, throwError)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified System.Console.ANSI as ANSI

--------------------------------------
-- Basic statistics
--------------------------------------

-- | Create a frequency map.
freqs :: (Foldable f, Ord a) => f a -> Map a Int
freqs = M.fromListWith (+) . fmap (,1) . toList

--------------------------------------
-- Parsing and string handling
--------------------------------------

-- | Strip trailing and leading whitepsace
strip :: String -> String
strip = T.unpack . T.strip . T.pack

-- | Strip trailing newlines
stripNewlines :: String -> String
stripNewlines = reverse . dropWhile (== '\n') . reverse

--------------------------------------
-- Conversions
--------------------------------------

{- | Convert an 'Either' into a 'Maybe' (or any other 'Alternative' instance),
 dropping the error value.
-}
eitherToMaybe :: Alternative m => Either e a -> m a
eitherToMaybe = either (const empty) pure

{- | Conver a 'Maybe' into an 'Either' (or any other 'MonadError' instance),
 using the provided error value if necessary.
-}
maybeToEither :: MonadError e m => e -> Maybe a -> m a
maybeToEither e = maybe (throwError e) pure

--------------------------------------
-- Output
--------------------------------------

-- | Run som IO with the specified foreground colour and intensity.
withColor :: ANSI.ColorIntensity -> ANSI.Color -> IO () -> IO ()
withColor ci c io = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ci c]
  io
  ANSI.setSGR [ANSI.Reset]
