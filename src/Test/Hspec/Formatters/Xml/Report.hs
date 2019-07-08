-- |XML formatting for recroded test results.
module Test.Hspec.Formatters.Xml.Report where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (Sum), getSum)
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import Test.Hspec.Formatters
  ( FailureReason (Error, ExpectedButGot, NoReason, Reason)
  , FormatM
  , Formatter
  , footerFormatter
  , formatException
  , silent
  )
import Test.Hspec.Runner (Path)
import Text.Blaze (Markup, toMarkup, toValue, (!))
import Text.Blaze.Internal (customAttribute, customParent)
import Text.Blaze.Renderer.String (renderMarkup)

import Test.Hspec.Formatters.Xml.Recording
  ( RecordedTestActions (RecordedTestActions)
  , TestResult (TestResultFailure, TestResultSkipped, TestResultSuccess)
  , formatSeconds
  , recordedTestActionsResults
  , recordedTestActionsStartTime
  )

-- |"Formatter" that writes a summary of test results in Jenkins-friendly XML via the provided
-- action, which is executed just once after all tests complete. That's necessary because the root
-- node of the result includes summary info. Nothing is written directly to Hspec's output stream.
xmlFormatter :: Text -> (Text -> FormatM ()) -> RecordedTestActions -> Formatter
xmlFormatter name write RecordedTestActions{..} = silent {
  footerFormatter = do
    (results, output) <- recordedTestActionsResults
    let testCount = length results
        failureCount = getSum $ foldMap countFailure results
        errorCount = getSum $ foldMap countError results
        totalElapsed = getSum $ foldMap (\ (_, _, seconds) -> Sum seconds) results
        markup = testSuite testCount errorCount failureCount name totalElapsed recordedTestActionsStartTime
          $ foldMap formatResult results
            <> "\n"
            <> systemOut output
    write $ "<?xml version='1.0' encoding='UTF-8'?>\n" <> pack (renderMarkup markup)
  }
  where
    countFailure (_, (TestResultFailure (Error _ _)), _) = Sum 0
    countFailure (_, (TestResultFailure _), _)           = Sum 1
    countFailure _                                       = Sum 0
    countError (_, (TestResultFailure (Error _ _)), _) = Sum 1
    countError _                                       = Sum 0

formatResult :: (Path, TestResult, Double) -> Markup
formatResult (path, result, elapsed) = "\n" <> case result of
  TestResultSuccess ->
    testCase path elapsed ""
  TestResultFailure NoReason ->
    testCase path elapsed $ indent $ failure "NoReason" "<no reason given>" ""
  TestResultFailure (Reason reason) ->
    testCase path elapsed $ indent $ failure "Reason" (pack reason) ""
  TestResultFailure (ExpectedButGot preface expected actual) ->
    let message = maybe "" (<> "; ") preface
          <> "expected: " <> show expected
          <> "; actual: " <> show actual
    in testCase path elapsed $ indent $ failure "ExpectedButGot" (pack message) ""
  TestResultFailure (Error preface exc) ->
    testCase path elapsed $ indent $ error_ "Error" (pack $ fromMaybe "" preface) (pack $ formatException exc)
  TestResultSkipped desc ->
    testCase path elapsed $ indent $ skipped (pack $ fromMaybe "" desc) ""
  where
    indent = ("\n  " <>)

failure :: Text -> Text -> Text -> Markup
failure type_ message data_ = customParent "failure"
  ! customAttribute "type" (toValue type_)
  ! customAttribute "message" (toValue message)
  $ toMarkup data_

error_ :: Text -> Text -> Text -> Markup
error_ type_ message data_ = customParent "error"
  ! customAttribute "type" (toValue type_)
  ! customAttribute "message" (toValue message)
  $ toMarkup data_

skipped :: Text -> Text -> Markup
skipped message data_ = customParent "skipped"
  ! customAttribute "message" (toValue message)
  $ toMarkup data_

systemOut :: Text -> Markup
systemOut output = customParent "system-out"
  $ toMarkup output

systemErr :: Text -> Markup
systemErr output = customParent "system-err"
  $ toMarkup output

-- Children: failure? error? skipped?
testCase :: Path -> Double -> Markup -> Markup
testCase (groups, req) time = customParent "testcase"
  ! customAttribute "classname" (toValue . intercalate "." $ groups)
  ! customAttribute "name" (toValue req)
  ! customAttribute "time" (toValue . formatSeconds $ time)

-- Children: testcase* system-out? system-err?
testSuite :: Int -> Int -> Int -> Text -> Double -> UTCTime -> Markup -> Markup
testSuite tests errors failures name time timestamp =
  customParent "testsuite"
    ! customAttribute "tests" (toValue tests)
    ! customAttribute "errors" (toValue errors)
    ! customAttribute "failures" (toValue failures)
    ! customAttribute "name" (toValue name)
    ! customAttribute "hostname" "localhost"
    ! customAttribute "time" (toValue . formatSeconds $ time)
    ! customAttribute "timestamp" (toValue $ formatTime defaultTimeLocale (iso8601DateFormat $ Just "%T") timestamp)
