module Scraper
  ( ScraperContext(..)
  , scrapeRapperEmails
  )
  where

import Scraper.Email
import Scraper.SearchResult
import TwitterAuth
import User

import Protolude

import qualified Data.Generics.Product                as GLens
import qualified Web.Twitter.Conduit                  as Twitter
import qualified Web.Twitter.Conduit.Request.Internal as Twitter
import qualified Web.Twitter.Types                    as Twitter

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
      , tweetCount   = ProcessedTweetCount 0
      , nextToken    = Nothing
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
      , tweetCount   :: ProcessedTweetCount
      , nextToken    :: Maybe NextPageToken
      }

scrape :: (MonadReader ScraperContext m, MonadIO m) => ScrapeNextRequest -> m ()
scrape ScrapeNextRequest{..} = do
  putStrLn @Text
    $  "Starting request #" <> show requestCount

  SearchResults{..} <- call nextQuery

  let currentTweetCount = ProcessedTweetCount (toInteger $ length results)
      processedTweetCount = tweetCount + currentTweetCount
  putStrLn @Text $ "Received " <> show currentTweetCount <> " tweets"

  extractEmails results

  putStrLn @Text "Request processing complete. Email search and extraction completed."
  putStrLn @Text $ "I have processed " <> show processedTweetCount <> " tweets in total"

  proceedIfNotEmpty ProceedRequest{..} $ \token ->
    scrape ScrapeNextRequest
      { requestCount = requestCount + 1
      , tweetCount   = processedTweetCount
      , nextToken    = Just token
      }

  where
    Twitter.APIRequest{..} = searchQuery

    params = case nextToken of
      Nothing    -> _params
      Just token ->
        ("next", Twitter.PVString $ getNextPageToken token) : _params

    nextQuery
      = searchQuery { Twitter._params = params }



searchQuery :: Twitter.APIRequest Twitter.SearchTweets SearchResults
searchQuery
  = Twitter.APIRequest
      { _method = "GET"
      , _url    = "https://api.twitter.com/1.1/tweets/search/fullarchive/prod.json"
      , _params = [("query", Twitter.PVString "(from:SendBeatsBot)")]
      }


data ProceedRequest
  = ProceedRequest
      { processedTweetCount :: ProcessedTweetCount
      , next                :: Maybe NextPageToken
      }

proceedIfNotEmpty
  :: ( MonadIO m
     , MonadReader context m
     , TargetTweetCount `GLens.HasType` context
     )
  => ProceedRequest
  -> (NextPageToken -> m ())
  -> m ()

proceedIfNotEmpty ProceedRequest{..} nextAction
  = case next of
      Nothing -> do
        putStrLn @Text
          "\nTwitter API returned no tweets for this request. End of scraping.\n"

        targetTweetCount <- GLens.getTyped @TargetTweetCount <$> ask

        when (show @_ @Text processedTweetCount /= show targetTweetCount)
          $ putStrLn @Text "WARNING!"

        putStrLn @Text
          $  "Target tweet count was " <> show targetTweetCount <> ".\n"
          <> "I processed " <> show processedTweetCount <> ".\n"

      Just nextPageToken -> do
        putStrLn @Text ""
        nextAction nextPageToken
