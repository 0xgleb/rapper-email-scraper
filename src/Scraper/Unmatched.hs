{-# LANGUAGE BlockArguments #-}

module Scraper.Unmatched
  ( UnmatchedTweet(..)
  , saveUnmatchedTweets

  , SavedTweets(..)
  )
  where

import qualified Prelude
import           Protolude
import JSONFileManager

import qualified Data.Aeson           as Aeson
import qualified Data.HashMap.Strict  as Map
import qualified Data.Text            as Txt
import qualified Web.Twitter.Types    as Twitter


saveUnmatchedTweets :: MonadJSONFileManager m => [UnmatchedTweet] -> m ()
saveUnmatchedTweets unmatchedTweets = do
  let filePath = "unmatched-tweets.json"

  fileExists <- doesFileExist filePath

  let savedData
        | not fileExists = pure $ Just $ SavedTweets []
        | otherwise      = readJSONFile filePath

  savedTweets <- savedData >>= \case
    Just (SavedTweets savedTweets) ->
      pure savedTweets

    Nothing -> do
      Prelude.error $ "Couldn't parse " <> filePath

  writeJSONFile filePath $ SavedTweets $ savedTweets <> unmatchedTweets

data UnmatchedTweet
  = UnmatchedTweet
      { statusId  :: Twitter.StatusId
      , tweetText :: Text
      }
  deriving stock (Generic, Show, Eq)

newtype SavedTweets
  = SavedTweets { unSavedTweets :: [UnmatchedTweet] }
  deriving newtype (Show, Eq)

instance Aeson.ToJSON SavedTweets where
  toJSON (SavedTweets tweets)
    = Aeson.Object $ flip foldMap tweets $ \UnmatchedTweet{..} ->
        Map.singleton (show statusId) $ Aeson.String tweetText

instance Aeson.FromJSON SavedTweets where
  parseJSON
    = Aeson.withObject "unmatched tweets"
    $ fmap (\(SavedTweets tweets) -> SavedTweets $ sortOn (negate . statusId) tweets)
    . flip Map.foldrWithKey (pure $ SavedTweets [])
        \unparsedIdTxt text accumulator -> do

          let unparsedId = Txt.unpack unparsedIdTxt

          statusId <-
            maybe (Prelude.fail $ "Couldn't parse integer " <> unparsedId) pure
              $ readMaybe unparsedId

          tweetText <- Aeson.parseJSON text

          SavedTweets tweets <- accumulator

          pure $ SavedTweets $ tweets <> [UnmatchedTweet{..}]
