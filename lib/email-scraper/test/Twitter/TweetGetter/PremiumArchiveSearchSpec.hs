module Twitter.TweetGetter.PremiumArchiveSearchSpec (spec) where

import Twitter.TweetGetter.Mock
import Twitter.TweetGetter.MonadRapperTweetsGetter
import Twitter.TweetGetter.PremiumArchiveSearch

import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "getPremiumArchiveSearchTweets" $ do
    it "works with mock instances" $ do
      let wrappedResult = tweets <$> getRapperTweets (PremiumArchiveSearch Nothing Nothing)
          result        = runIdentity $ runMockedCallT wrappedResult
      result `shouldBe` mockTweets
