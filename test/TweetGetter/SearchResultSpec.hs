module TweetGetter.SearchResultSpec (spec) where

import TweetGetter.SearchResult

import Protolude
import Test.Hspec

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BSL

spec :: Spec
spec = do
  describe "Aeson.decode @SearchResults" $ do
    it "can decode an example search response" $ do
      bytestring <- BSL.readFile "./resources/search-result-example.json"
      Aeson.decode @SearchResults bytestring `shouldNotBe` Nothing
