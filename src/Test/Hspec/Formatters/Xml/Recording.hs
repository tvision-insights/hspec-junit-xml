-- |
module Test.Hspec.Formatters.Xml.Recording where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (foldl')
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Test.Hspec.Formatters
  ( FailureReason
  , FormatM
  , Formatter (Formatter)
  , Seconds (Seconds)
  , exampleFailed
  , exampleGroupDone
  , exampleGroupStarted
  , examplePending
  , exampleProgress
  , exampleSucceeded
  , failedFormatter
  , footerFormatter
  , getRealTime
  , headerFormatter
  , silent
  , specdoc
  , withFailColor
  , withPendingColor
  , withSuccessColor
  , writeLine
  )
import Test.Hspec.Runner (Path)
import Text.Printf (printf)

data TestResult
  = TestResultSuccess
  | TestResultFailure FailureReason
  | TestResultSkipped (Maybe String)
  deriving (Show)

data FormatterState = FormatterState
  { formatterStateLastReset :: Double
  -- ^Real time when the timer was last reset, in terms of seconds since the test run started.
  , formatterStateResults   :: [(Path, TestResult, Double)]
  -- ^List of results with elapsed times, in reverse chronological order.
  , formatterStateOutput    :: Text
  -- ^Output written by all the tests.
  } deriving (Show)

data RecordedTestActions = RecordedTestActions
  { recordedTestActionsElapsed   :: FormatM Double
  -- ^Elapsed time for the test which just finished.
  , recordedTestActionsStartTime :: UTCTime
  -- ^The time the tests started running. Actually, the time the "recording" formatter
  -- was constructed. This doesn't really relate to test times at all, but is recorded because it's
  -- useful to 'xmlFormatter'.
  , recordedTestActionsResults   :: FormatM ([(Path, TestResult, Double)], Text)
  -- ^Results for all tests seen so far, and all of their output in a single block of text.
  }

-- |Compose multiple HSpec formatters, which may required 'RecordedTestActions', in sequence.
composeFormatters :: [(RecordedTestActions -> Formatter)] -> IO Formatter
composeFormatters fs = do
  (recorder, actions) <- recordingFormatter
  pure $ foldl' (\ acc f -> teeFormatter acc (f actions)) recorder fs

-- |Hspec "Formatter" that doesn't actually produce any output, but captures information about the
-- the test execution that can be used to generate whatever output you like.
recordingFormatter :: IO (Formatter, RecordedTestActions)
recordingFormatter = do
  stateRef <- newIORef $ FormatterState 0 [] ""
  startTime <- getCurrentTime
  let resetTimer :: FormatM ()
      resetTimer = do
        Seconds ts <- getRealTime
        liftIO $ modifyIORef' stateRef $ \ s -> s { formatterStateLastReset = ts }

      getElapsedRealTime :: FormatM Double
      getElapsedRealTime = do
        Seconds endTs <- getRealTime
        startTs <- liftIO $ formatterStateLastReset <$> readIORef stateRef
        pure $ endTs - startTs

      writeResult :: Path -> TestResult -> Double -> FormatM ()
      writeResult path result elapsed =
        liftIO $ modifyIORef' stateRef $ \ s ->
          s { formatterStateResults = (path, result, elapsed) : formatterStateResults s }

      writeOutput :: Text -> FormatM ()
      writeOutput output =
        liftIO $ modifyIORef' stateRef $ \ s ->
          s { formatterStateOutput = formatterStateOutput s <> ("\n" <> output) }

      formatter = Formatter
        { headerFormatter =
            pure ()
        , exampleGroupStarted = \ _groups _req ->
            resetTimer
        , exampleGroupDone =
            pure ()
        , exampleProgress = \ _path _progress ->
            pure ()
        , exampleSucceeded = \ path output -> do
            elapsed <- getElapsedRealTime
            writeResult path TestResultSuccess elapsed
            writeOutput (pack output)
            resetTimer
        , exampleFailed = \ path output err -> do
            elapsed <- getElapsedRealTime
            writeResult path (TestResultFailure err) elapsed
            writeOutput (pack output)
            resetTimer
        , examplePending = \ path output mdesc -> do
            elapsed <- getElapsedRealTime
            writeResult path (TestResultSkipped mdesc) elapsed
            writeOutput (pack output)
            resetTimer
        , failedFormatter =
            -- Here's where you get the location of any failure, which you could capture so you
            -- could add locations to each failure/error node. You would want that if you were going
            -- to use _only_ this formatter, but currently we're also running the default formatter
            -- and only using this formatter to capture a second stream of information (mainly
            -- timing.)
            -- failed <- getFailMessages
            -- forM_ failed $ \ (FailureRecord loc path reason) ->
            --   TODO
            pure ()
        , footerFormatter =
            pure ()
        }
      recordedTestActionsElapsed = do
        state <- liftIO $ readIORef stateRef
        pure $ case formatterStateResults state of
          (_, _, seconds) : _ -> seconds
          []                  -> 0.0
      recordedTestActionsStartTime = startTime
      recordedTestActionsResults = do
        state <- liftIO $ readIORef stateRef
        let results = reverse $ formatterStateResults state
            output = formatterStateOutput state
        pure (results, output)
  pure (formatter, RecordedTestActions {..})

-- |Hspec's 'specdoc', with an additional line after each test showing its elapsed time.
specdocWithElapsedTimes :: RecordedTestActions -> Formatter
specdocWithElapsedTimes RecordedTestActions{..} = teeFormatter specdoc timeAnnotator
  where
    timeAnnotator = silent
      { exampleSucceeded = \ path _   -> withSuccessColor $ timeMessage path
      , exampleFailed    = \ path _ _ -> withFailColor $ timeMessage path
      , examplePending   = \ path _ _ -> withPendingColor $ timeMessage path
      }
    timeMessage (groups, _) = do
      elapsed <- recordedTestActionsElapsed
      writeLine $ indentationFor groups <> "time: " <> formatSeconds elapsed <> "s"
    indentationFor groups = replicate ((length groups + 1) * 2) ' '

-- |Format seconds with three decimals, so it's easier to compare times at a glance.
formatSeconds :: Double -> String
formatSeconds = printf "%.3f"

-- |Send each event to both formatters, first the left and then the right.
teeFormatter :: Formatter -> Formatter -> Formatter
teeFormatter f1 f2 = Formatter
  { headerFormatter = compose0 headerFormatter
  , exampleGroupStarted = compose2 exampleGroupStarted
  , exampleGroupDone = compose0 exampleGroupDone
  , exampleProgress = compose2 exampleProgress
  , exampleSucceeded = compose2 exampleSucceeded
  , exampleFailed = compose3 exampleFailed
  , examplePending = compose3 examplePending
  , failedFormatter = compose0 failedFormatter
  , footerFormatter = compose0 footerFormatter
  }
  where
    compose0 p = p f1 >> p f2
    compose2 :: (Formatter -> (a -> b -> FormatM ())) -> (a -> b -> FormatM ())
    compose2 p = \ x y -> p f1 x y >> p f2 x y
    compose3 :: (Formatter -> (a -> b -> c -> FormatM ())) -> (a -> b -> c -> FormatM ())
    compose3 p = \ x y z -> p f1 x y z >> p f2 x y z
