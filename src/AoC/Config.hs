module AoC.Config (
  Config (..),
  defConfPath,
  readConfig,
)
where

import Control.Exception
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Default.Class
import qualified Data.Yaml as Y
import GHC.Generics (Generic)
import System.IO.Error
import Text.Printf

-- | Configuration data.
data Config = Config
  { session :: Maybe String
  -- ^ default: 'Nothing'
  , year :: Integer
  -- ^ default: 2020
  }
  deriving (Generic, Show)

-- | Default configuration.
instance Default Config where
  def = Config{session = Nothing, year = 2020}

-- | Default configuration file path.
defConfPath :: FilePath
defConfPath = ".aoc-cfg.yaml"

-- | Read in the given config file.
readConfig :: FilePath -> IO Config
readConfig fp = do
  cfgInp <- tryJust (guard . isDoesNotExistError) $ BS.readFile fp
  case cfgInp of
    Left _ -> do
      Y.encodeFile @Config fp def
      return def
    Right bs -> case Y.decodeEither' bs of
      Left e -> do
        printf "Configuration file '%s' could not be parsed:\n" fp
        print e
        return def
      Right cfg -> return cfg

instance A.ToJSON Config where
  toJSON = A.genericToJSON A.defaultOptions
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.FromJSON Config where
  parseJSON = A.genericParseJSON A.defaultOptions
