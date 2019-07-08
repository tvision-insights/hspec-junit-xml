-- |Adapted from the abandoned hspec-jenkins package.
module Test.Hspec.Formatters.Xml
  ( hspecWithElapsedTimes
  , hspecWithElapsedTimesAndReport
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (hPut)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, withArgs)
import System.IO (IOMode (WriteMode), withFile)
import Test.Hspec (Spec)
import Test.Hspec.Runner (configFormatter, defaultConfig, evaluateSummary, readConfig, runSpec)

import Test.Hspec.Formatters.Xml.Recording (composeFormatters, specdocWithElapsedTimes)
import Test.Hspec.Formatters.Xml.Report (xmlFormatter)

-- |Run a Spec, showing elapsed times for each test, and also writing a report under the given
-- directory path. This report can be captured by tools like CircleCI to collect statistics
-- about which tests are failing and which tests are slow.
-- Hspec's 'specdoc' formatter is always used (ignoring configuration from the command-line), and
-- the elapsed time for each test is also written to the HSpec output stream.
-- Note: Timing data is meaningful only if no single test process runs any tests in parallel.
hspecWithElapsedTimesAndReport :: FilePath -> Text -> Spec -> IO ()
hspecWithElapsedTimesAndReport dir package spec = do
  createDirectoryIfMissing False dir
  let path = dir <> "/" <> unpack package <> ".xml"
  summary <- withFile path WriteMode $ \ h -> do
    formatter <- composeFormatters
      [ specdocWithElapsedTimes
      , xmlFormatter package (liftIO . hPut h . encodeUtf8)
      ]
    args <- getArgs
    cfg <- readConfig defaultConfig args
    withArgs [] $ runSpec spec cfg { configFormatter = Just formatter }
  evaluateSummary summary

-- |Run a 'Spec', using Hspecs' 'specdoc' formatter, but also showing the elapsed time for each test.
-- Note: Timing data is meaningful only if no single test process runs any tests in parallel.
hspecWithElapsedTimes :: Spec -> IO ()
hspecWithElapsedTimes spec = do
  formatter <- composeFormatters [specdocWithElapsedTimes]
  args <- getArgs
  cfg <- readConfig defaultConfig args
  summary <- withArgs [] $ runSpec spec cfg { configFormatter = Just formatter }
  evaluateSummary summary
