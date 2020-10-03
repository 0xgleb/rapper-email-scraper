module Scraper
  ( ScraperContext(..)
  , scrapeRapperEmails
  )
  where

import Scraper.Email
import TwitterAuth
import User

import Protolude

import qualified Data.Generics.Product                as GLens
import qualified Web.Twitter.Conduit                  as Twitter
import qualified Web.Twitter.Conduit.Request.Internal as Twitter
import qualified Web.Twitter.Types                    as Twitter
-- import qualified Web.Twitter.Conduit.Status           as Twitter

data ScraperContext
  = ScraperContext
      { session          :: !Session
      , userId           :: !Twitter.UserId
      , targetTweetCount :: !TargetTweetCount
      }
  deriving stock (Generic)

scrapeRapperEmails :: (MonadReader ScraperContext m, MonadIO m) => m ()
scrapeRapperEmails
  = scrape ScrapeNextRequest
      { requestCount = RequestCount 1
      , mbMaxId      = Nothing
      , tweetCount   = ProcessedTweetCount 0
      }


newtype ProcessedTweetCount
  = ProcessedTweetCount Integer
  deriving newtype (Show, Num)

newtype RequestCount
  = RequestCount Integer
  deriving newtype (Show, Num)

data ScrapeNextRequest
  = ScrapeNextRequest
      { requestCount :: RequestCount
      , mbMaxId      :: Maybe Twitter.StatusId
      , tweetCount   :: ProcessedTweetCount
      }

scrape :: (MonadReader ScraperContext m, MonadIO m) => ScrapeNextRequest -> m ()
scrape ScrapeNextRequest{..} = do
  putStrLn @Text
    $  "Starting request #" <> show requestCount
    <> case mbMaxId of
         Just maxId -> " with max_id=" <> show maxId
         Nothing    -> ""

  Twitter.SearchResult{..} <- call nextQuery

  let statuses                   = searchResultStatuses
      Twitter.SearchMetadata{..} = searchResultSearchMetadata


  let currentTweetCount = ProcessedTweetCount (toInteger $ length statuses)
      processedTweetCount = tweetCount + currentTweetCount
  putStrLn @Text $ "Received " <> show currentTweetCount <> " tweets"

  extractEmails statuses

  putStrLn @Text "Request processing complete. Email search and extraction completed."
  putStrLn @Text $ "I have processed " <> show processedTweetCount <> " tweets in total"

  proceedIfNotEmpty ProceedRequest{..} $ \Twitter.Status{..} ->
    scrape ScrapeNextRequest
      { requestCount = requestCount + 1
      , tweetCount   = processedTweetCount
      , mbMaxId      = Just $ statusId - 1
      }

  where
    Twitter.APIRequest{..} = searchQuery

    params = case mbMaxId of
      Nothing    -> _params
      Just maxId -> ("max_id", Twitter.PVInteger maxId) : _params

    nextQuery
      = searchQuery { Twitter._params = params }


searchQuery :: Twitter.APIRequest Twitter.SearchTweets (Twitter.SearchResult [Twitter.Status])
searchQuery
  = Twitter.searchTweets "(from:SendBeatsBot) -filter:replies"


-- searchQuery :: Twitter.APIRequest Twitter.StatusesUserTimeline [Twitter.Status]
-- searchQuery = query
--   { Twitter._params = ("count", Twitter.PVInteger 200) : Twitter._params query }

--   where
--     query = Twitter.userTimeline sendBeatsBotHandle


data ProceedRequest
  = ProceedRequest
      { processedTweetCount :: ProcessedTweetCount
      , statuses            :: [Twitter.Status]
      }

proceedIfNotEmpty
  :: ( MonadIO m
     , MonadReader context m
     , TargetTweetCount `GLens.HasType` context
     )
  => ProceedRequest
  -> (Twitter.Status -> m ())
  -> m ()

proceedIfNotEmpty ProceedRequest{..} nextAction
  = case headMay $ sortOn Twitter.statusId statuses of
      Nothing -> do
        putStrLn @Text
          "\nTwitter API returned no tweets for this request. End of scraping.\n"

        targetTweetCount <- GLens.getTyped @TargetTweetCount <$> ask

        when (show @_ @Text processedTweetCount /= show targetTweetCount)
          $ putStrLn @Text "WARNING!"

        putStrLn @Text
          $  "Target tweet count was " <> show targetTweetCount <> ".\n"
          <> "I processed " <> show processedTweetCount <> ".\n"

      Just status -> do
        putStrLn @Text ""
        nextAction status
