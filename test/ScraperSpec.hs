module ScraperSpec (spec) where

import Scraper
import Util

import TestMonad

import qualified Twitter as Tw

import Protolude
import Test.Hspec

import qualified Data.Map.Strict as Map

scraperContext :: ScraperContext
scraperContext
  = ScraperContext
      { userId           = 1234
      , targetTweetCount = Tw.TargetTweetCount 1234
      }

spec :: Spec
spec = do
  describe "scrapeRapperEmails" $ do
    it "does things" $ do
      let TestResult{..} = runTestMonad scraperContext $ do
            scrapeRapperEmails (Free $ Tw.FreeSearch Nothing) Nothing
      fileSystem `shouldBe` Map.fromList
        [ ("rapper-emails.txt"    , "test@email.com\ntest@gmail.com\n")
        , ("unmatched-tweets.json", "{\"983741923841\":\"Don't send me beats\"}")
        ]

  describe "proceedIfNotEmpty" $ do
    it "calls the passed action if hasNext == True" $ do
      let TestResult{..}
            = runTestMonad ((Tw.TargetTweetCount 1234), ())
            $ proceedIfNotEmpty @(Tw.TargetTweetCount, ())
                (ProceedIfNotEmptyArgs
                  { processedTweetCount = ProcessedTweetCount 1234
                  , hasNext             = True
                  , minimumId           = 12341234132
                  })
                (say "hello")

      output `shouldBe` ["", "hello"]

    it "doesn't call the passed action and says right things if hasNext == False" $ do
      let TestResult{..}
            = runTestMonad ((Tw.TargetTweetCount 1234), ())
            $ proceedIfNotEmpty @(Tw.TargetTweetCount, ())
                (ProceedIfNotEmptyArgs
                  { processedTweetCount = ProcessedTweetCount 123
                  , hasNext             = False
                  , minimumId           = 12341234132
                  })
                (say "hello")

      output `shouldBe`
        [ "\nTwitter API returned no tweets for this request. End of scraping.\n"
        , "WARNING!"
        , "Target tweet count was 1234.\nI processed 123.\n"
        , "Oldest processed tweet was created at 2020-11-03"
        ]
