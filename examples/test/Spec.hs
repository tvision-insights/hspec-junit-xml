import Control.Concurrent (threadDelay)
import Test.Hspec (Spec, describe, example, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (property)

import Test.Hspec.Formatters.Xml

main :: IO ()
main = do
  -- hspecWithElapsedTimes $
  --   describe "with times" spec

  hspecWithElapsedTimesAndReport "test-results" "examples" $
    describe "with times and report" spec

spec :: Spec
spec = do
  it "succeeds, quickly" $ do
    threadDelay 2_000
    pure ()

  it "succeeds, not so fast" $ do
    threadDelay 1_000_000
    pure ()

  it "fails" $ do
    threadDelay 3_000
    "an actual String" `shouldBe` ("the String we said we expected" :: String)

  it "errors" $ example $ do
    threadDelay 4_000
    fail "an exception in IO"

  it "succeeds, with arbitrary input" $ property $ \ (x :: Int) -> do
    threadDelay 100
    x `shouldBe` x

  it "fails, with arbitrary input" $ property $ \ (x :: Int) -> do
    threadDelay 200
    x `shouldSatisfy` (< 10)
