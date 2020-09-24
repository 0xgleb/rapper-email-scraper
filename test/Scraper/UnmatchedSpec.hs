{-# OPTIONS_GHC -Wno-orphans #-}

module Scraper.UnmatchedSpec (spec) where

import Scraper.Duplicates
import Scraper.Unmatched

import Protolude
import Test.Hspec
import Test.QuickCheck

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
    it "fromJSON . toJSON == identity" $ property $ \(SavedTweets savedTweets :: SavedTweets) -> do
      let hackSavedTweets = SavedTweets $ removeDuplicates statusId savedTweets

      Aeson.decode (Aeson.encode hackSavedTweets) `shouldBe` Just hackSavedTweets
