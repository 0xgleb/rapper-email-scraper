module ScraperSpec (spec) where

import Scraper
import Util

import TestMonad

import qualified Twitter as Tw

import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "proceedIfNotEmpty" $ do
    it "calls the passed action if hasNext == True" $ do
      let (_, msgs)
            = runTestMonad ((Tw.TargetTweetCount 1234), ())
            $ proceedIfNotEmpty @(Tw.TargetTweetCount, ())
                (ProceedIfNotEmptyArgs
                  { processedTweetCount = ProcessedTweetCount 1234
                  , hasNext             = True
                  , minimumId           = 12341234132
                  })
                (say "hello")

      msgs `shouldBe` ["", "hello"]
