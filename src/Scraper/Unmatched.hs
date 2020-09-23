{-# LANGUAGE DeriveAnyClass #-}

module Scraper.Unmatched
  ( UnmatchedTweet(..)
  , saveUnmatchedTweet
  )
  where

import qualified Prelude
import           Protolude

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified System.Directory     as Dir
import qualified Web.Twitter.Types    as Twitter

saveUnmatchedTweet :: MonadIO m => UnmatchedTweet -> m ()
saveUnmatchedTweet unmatchedTweets = do
  let filePath = "unmatched-tweets.json"

  fileExists <- liftIO $ Dir.doesFileExist filePath

  let savedData
        | not fileExists
        = pure $ Just []

        | otherwise
        = liftIO $ fmap Aeson.decode $ BSL.readFile filePath

  savedTweets <- savedData >>= \case
    Just savedTweets ->
      pure savedTweets

    Nothing -> do
      Prelude.error $ "Couldn't parse " <> filePath

  liftIO $ BSL.writeFile filePath
    $ Aeson.encode $ savedTweets <> [unmatchedTweets]

data UnmatchedTweet
  = UnmatchedTweet
      { statusId  :: Twitter.StatusId
      , tweetText :: Text
      }
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)
