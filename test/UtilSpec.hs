module UtilSpec (spec) where

import Util

import Protolude
import Test.Hspec

import qualified Data.Time as Time

spec :: Spec
spec = do
  describe "dayToTwitterTime" $ do
    it "converts dates to the format twitter expects" $ do
      let dateToTime y m d = Time.UTCTime (Time.fromGregorian y m d) 0

      dayToTwitterTime (dateToTime 2015 12 22) `shouldBe` "201512220000"

      dayToTwitterTime (dateToTime 2017 3 1) `shouldBe` "201703010000"
