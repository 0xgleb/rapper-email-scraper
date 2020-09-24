module Scraper.DuplicatesSpec (spec) where

import Scraper.Duplicates
import Scraper.Unmatched

import Protolude
import Test.Hspec
import Test.QuickCheck

import qualified Prelude

spec :: Spec
spec = do
  describe "removeDuplicates" $ do
    it "doesn't have a bug" $ do
      let bugList =
            [ UnmatchedTweet {statusId = 4, tweetText = "#\1510t"}
            , UnmatchedTweet {statusId = 0, tweetText = ""}
            , UnmatchedTweet {statusId = -3, tweetText = "\336\65383"}
            , UnmatchedTweet {statusId = -3, tweetText = "5\864"}
            ]

      removeDuplicates statusId bugList `shouldBe` (take 3 bugList)

    it "removeDuplicate is idempotent" $ property $ \(randomList :: [Prelude.String]) ->
      removeDuplicates identity (removeDuplicates identity randomList)
        == removeDuplicates identity randomList

    it "removes duplicates" $ property $ \firstList secondList ->
      let duplicate n = (n :: Int, "duplicate" :: Prelude.String)

          firstItem = (1, "")

          randomList
            =  [firstItem, duplicate 1]
            <> firstList
            <> [duplicate 2, duplicate 3]
            <> secondList
            <> [duplicate 4]

          removeFirstItem = filter ((/= "") . snd)

          withoutDuplicates
            = firstItem
            : duplicate 1
            : removeFirstItem (firstList <> secondList)

      in removeDuplicates snd randomList `shouldBe` withoutDuplicates
