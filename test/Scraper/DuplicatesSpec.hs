module Scraper.DuplicatesSpec (spec) where

import Scraper.Duplicates

import Protolude
import Test.Hspec
import Test.QuickCheck

import qualified Prelude

spec :: Spec
spec = do
  describe "removeDuplicates" $ do
    it "removeDuplicate is idempotent" $ property $ \(randomList :: [Prelude.String]) ->
      removeDuplicates identity (removeDuplicates identity randomList)
        == removeDuplicates identity randomList

    it "removes duplicates" $ property $ \firstList secondList ->
      let duplicate n = (n :: Int, "duplicate" :: Prelude.String)

          randomList
            =  [duplicate 1] <> firstList
            <> [duplicate 2, duplicate 3]
            <> secondList <> [duplicate 4]

          withoutDuplicates
            = duplicate 1 : firstList <> secondList

      in removeDuplicates snd randomList == withoutDuplicates
