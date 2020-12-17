module Twitter.TweetGetter.FreeSearchSpec (spec) where

import Twitter.TweetGetter.FreeSearch
import Twitter.TweetGetter.MonadRapperTweetsGetter

import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "getFreeSearchTweets" $ do
    it "works with mock instances" $ do
      let wrappedResult = tweets <$> getRapperTweets (FreeSearch Nothing)
          result        = runIdentity $ runMockedCallT wrappedResult
      result `shouldBe` mockTweets
