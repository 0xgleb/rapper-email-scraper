{-# OPTIONS_GHC -Wno-orphans #-}

module FileManagerSpec (spec) where

import FileManager

import Protolude hiding (list)
import Test.Hspec

import qualified Data.Map.Strict as Map
import           Data.Text.Arbitrary ()

spec :: Spec
spec = do
  describe "instance MonadJSONFileManager StateJSONFileManager" $ do
    it "works" $ do
      let result = fst $ flip runState (Map.fromList []) $ runStateFileManager $ do
            writeJSONFile "unmatched-tweets.json" [1 :: Word, 2, 3, 4]
            fileExists <- doesFileExist "unmatched-tweets.json"
            (list :: Maybe [Word]) <- readJSONFile "unmatched-tweets.json"
            pure (fileExists, list)
      result `shouldBe` (True, Just [1, 2, 3, 4])
