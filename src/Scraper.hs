module Scraper
  ( ScraperContext(..)
  , TweetId(..)
  , Mode(..)
  , scrapeRapperEmails
  )
  where

import Scraper.Email
import TweetGetter
import TwitterAuth
import User

import Protolude

import qualified Data.Generics.Product as GLens
import qualified Data.Time             as Time
import qualified Web.Twitter.Conduit   as Twitter
import qualified Web.Twitter.Types     as Twitter

data ScraperContext
  = ScraperContext
      { session          :: !Session
      , userId           :: !Twitter.UserId
      , targetTweetCount :: !TargetTweetCount
      }
  deriving stock (Generic)

newtype TweetId
  = TweetId { getTweetId :: Twitter.StatusId }

data Mode
  = Free FreeSearch
  | Premium PremiumTweetArchiveSearch

scrapeRapperEmails
  :: ( MonadReader ScraperContext m
     , MonadIO m
     )
  => Mode
  -> Maybe TweetId
  -> m ()

scrapeRapperEmails mode oldestProcessedId = do
  toDate <- forM oldestProcessedId $
    fmap Twitter.statusCreatedAt . call . Twitter.statusesShowId . getTweetId

  scrape ScrapeNextRequest
    { requestCount = RequestCount 1
    , tweetCount   = ProcessedTweetCount 0
    , request      = Nothing
    , toDate
    , mode
    }


newtype ProcessedTweetCount
  = ProcessedTweetCount Integer
  deriving newtype (Show, Num)

newtype RequestCount
  = RequestCount Integer
  deriving newtype (Show, Num)

data ScrapeNextRequest m
  = ScrapeNextRequest
      { requestCount :: RequestCount
      , tweetCount   :: ProcessedTweetCount
      , toDate       :: Maybe Time.UTCTime
      , request      :: Maybe (m (RequestResult m))
      , mode         :: Mode
      }

scrape :: (MonadReader ScraperContext m, MonadIO m) => ScrapeNextRequest m -> m ()
scrape ScrapeNextRequest{..} = do
  putStrLn @Text $ "Starting request #" <> show requestCount

  RequestResult{..} <- case request of
    Just req -> req
    Nothing -> case mode of
      Free freeRequest       -> getRapperTweets freeRequest
      Premium premiumRequest -> getRapperTweets premiumRequest

  let currentTweetCount   = ProcessedTweetCount (toInteger $ length tweets)
      processedTweetCount = tweetCount + currentTweetCount

      minimumId = minimum $ id <$> tweets

  putStrLn @Text
    $  "Received " <> show currentTweetCount
    <> " tweets with the lowest id=" <> show minimumId

  extractEmails tweets

  putStrLn @Text "Request processing complete. Email search and extraction completed."
  putStrLn @Text $ "I have processed " <> show processedTweetCount <> " tweets in total"

  let hasNext = isJust nextRequest

  proceedIfNotEmpty ProceedRequest{..}
    $ scrape ScrapeNextRequest
        { requestCount = requestCount + 1
        , tweetCount   = processedTweetCount
        , toDate       = Nothing
        , request      = nextRequest
        , mode
        }

data ProceedRequest
  = ProceedRequest
      { processedTweetCount :: ProcessedTweetCount
      , hasNext             :: Bool
      , minimumId           :: Twitter.StatusId
      }

proceedIfNotEmpty
  :: ( MonadIO m
     , MonadReader context m
     , TargetTweetCount `GLens.HasType` context
     , Session `GLens.HasType` context
     )
  => ProceedRequest
  -> m ()
  -> m ()

proceedIfNotEmpty ProceedRequest{..} nextAction
  | hasNext = do
      putStrLn @Text
        "\nTwitter API returned no tweets for this request. End of scraping.\n"

      targetTweetCount <- GLens.getTyped @TargetTweetCount <$> ask

      when (show @_ @Text processedTweetCount /= show targetTweetCount)
        $ putStrLn @Text "WARNING!"

      putStrLn @Text
        $  "Target tweet count was " <> show targetTweetCount <> ".\n"
        <> "I processed " <> show processedTweetCount <> ".\n"

      Twitter.Status{..} <- call $ Twitter.statusesShowId minimumId

      putStrLn @Text
        $ "Oldest processed tweet was created at " <> show statusCreatedAt

  | otherwise = do
      putStrLn @Text ""
      nextAction
