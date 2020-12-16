{-# OPTIONS_GHC -Wno-orphans #-}

module Scraper.UnmatchedSpec (spec) where

import JSONFileManager
import Scraper.Duplicates
import Scraper.Unmatched

import Protolude
import Test.Hspec
import Test.QuickCheck

import qualified Data.Map.Strict as Map
import qualified Data.Aeson          as Aeson
import           Data.Text.Arbitrary ()
import qualified Generic.Random      as RandGen

instance Arbitrary SavedTweets where
  arbitrary
    =   SavedTweets . sortOn (negate . statusId) . removeDuplicates statusId
    <$> arbitrary

instance Arbitrary UnmatchedTweet where
  arbitrary = RandGen.genericArbitraryU

spec :: Spec
spec = do
  describe "ToJSON & FromJSON" $ do
    it "fromJSON . toJSON == identity"
      $ property $ \(SavedTweets savedTweets) -> do
          let hackSavedTweets
                = SavedTweets $ removeDuplicates statusId savedTweets

          Aeson.decode (Aeson.encode hackSavedTweets)
            `shouldBe` Just hackSavedTweets

  describe "saveUnmatchedTweet @ReaderJSONFileManager" $ do
    it "can save a list of unmatched tweets and always read it back"
      $ property $ \(SavedTweets unmatchedTweets) ->
          let parsedUnmatchedTweets = fst $ flip runState (Map.fromList []) $ runStateJSONFileManager $ do
                writeJSONFile "unmatched-tweets.json" $ SavedTweets []
                saveUnmatchedTweets unmatchedTweets
                maybe Nothing (Just . unSavedTweets)
                  <$> readJSONFile "unmatched-tweets.json"
          in parsedUnmatchedTweets `shouldBe` Just unmatchedTweets
