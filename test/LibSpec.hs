module LibSpec (spec) where

import Lib

import Protolude
import Test.Hspec

import qualified Web.Twitter.Conduit.Request.Internal as Twitter

spec :: Spec
spec = do
  describe "extractMaxId" $ do
    it "can extract max_id query parameter from twitter response" $ do
      let exampleNextResults = "?max_id=1307926955614896127&q=%28from%3ASendBeatsBot%29%20-filter%3Alinks%20-filter%3Areplies&include_entities=1"
          expectedMaxId = Twitter.PVInteger 1307926955614896127

      extractMaxId exampleNextResults `shouldBe` Just expectedMaxId
