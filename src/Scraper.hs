{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

module Scraper
  ( ScraperContext(..)
  , ProcessedTweetCount(..)
  , TweetId(..)
  , Mode(..)
  , scrapeRapperEmails

  , MonadGetStatusById(..)
  , GetStatusByIdT(..)
  , MockedGetStatusByIdT(..)

  , ProceedIfNotEmptyArgs(..)
  , proceedIfNotEmpty
  )
  where

import           FileManager
import           Scraper.Email
import qualified Twitter       as Tw
import Twitter.TweetGetter.Mock
import           Util

import Protolude

import qualified Data.Generics.Product as GLens
import qualified Data.Generics.Product as Generic
import qualified Data.Time             as Time
import qualified Web.Twitter.Conduit   as Twitter
import qualified Web.Twitter.Types     as Twitter
import qualified Control.Monad.Trans as Trans

data ScraperContext
  = ScraperContext
      { manager          :: !Twitter.Manager
      , twInfo           :: !Twitter.TWInfo
      , userId           :: !Twitter.UserId
      , targetTweetCount :: !Tw.TargetTweetCount
      }
  deriving stock (Generic)

type HasScraperContext context m
  = ( MonadReader context m
    , ScraperContext `Generic.Subtype` context
    )

data Mode
  = Free !Tw.FreeSearch
  | Premium !Tw.PremiumArchiveSearch

newtype TweetId
  = TweetId { getTweetId :: Twitter.StatusId }

scrapeRapperEmails
  :: ( MonadReader ScraperContext m
     , Tw.MonadRapperTweetsGetter Tw.FreeSearch m
     , Tw.MonadRapperTweetsGetter Tw.PremiumArchiveSearch m
     , MonadGetStatusById m
     , MonadFileManager m
     , MonadSay m
     )
  => Mode
  -> Maybe TweetId
  -> m ()

scrapeRapperEmails mode oldestProcessedId = do
  toDate <- forM oldestProcessedId
    $ fmap Twitter.statusCreatedAt . getStatusById . getTweetId

  scrape ScrapeArgs
    { requestCount = RequestCount 1
    -- we're setting args for the 1st request here, hence the 1
    , tweetCount = ProcessedTweetCount 0
    -- no tweets have been processed at the start
    , request = Nothing
    -- use mode for the first request
    , toDate
    , mode
    }


data ScrapeArgs m
  = ScrapeArgs
      { requestCount :: !RequestCount
      , tweetCount   :: !ProcessedTweetCount
      , toDate       :: !(Maybe Time.UTCTime)
      , request      :: Maybe (m (Tw.RequestResult m))
      , mode         :: !Mode
      }

newtype RequestCount
  = RequestCount Integer
  deriving newtype (Show, Num)

newtype ProcessedTweetCount
  = ProcessedTweetCount Integer
  deriving newtype (Show, Num)

scrape
  :: forall context m
   . ( HasScraperContext context m
     , Tw.Session `GLens.Subtype` context
     , Tw.MonadRapperTweetsGetter Tw.FreeSearch m
     , Tw.MonadRapperTweetsGetter Tw.PremiumArchiveSearch m
     , MonadGetStatusById m
     , MonadFileManager m
     , MonadSay m
     )
  => ScrapeArgs m
  -> m ()
scrape ScrapeArgs{..} = do
  say $ "Starting request #" <> show requestCount

  Tw.RequestResult{..} <- case request of
    Just req -> req
    Nothing -> case mode of
      Free freeRequest       -> Tw.getRapperTweets freeRequest
      Premium premiumRequest -> Tw.getRapperTweets premiumRequest

  let currentTweetCount   = ProcessedTweetCount $ toInteger $ length tweets
      processedTweetCount = tweetCount + currentTweetCount

      minimumId = minimum $ Tw.id <$> tweets

  say
    $  "Received " <> show currentTweetCount
    <> " tweets with the lowest id=" <> show minimumId

  extractEmailsFromTweets @ScraperContext tweets

  say "Request processing complete. Email search and extraction completed."
  say $ "I have processed " <> show processedTweetCount <> " tweets in total"

  let hasNext = isJust nextRequest

  proceedIfNotEmpty @ScraperContext @context ProceedIfNotEmptyArgs{..}
    $ scrape ScrapeArgs
        { requestCount = requestCount + 1
        , tweetCount   = processedTweetCount
        , toDate       = Nothing
        , request      = nextRequest
        , mode
        }


data ProceedIfNotEmptyArgs
  = ProceedIfNotEmptyArgs
      { processedTweetCount :: !ProcessedTweetCount
      , hasNext             :: !Bool
      , minimumId           :: !Twitter.StatusId
      }

class Monad m => MonadGetStatusById m where
  getStatusById :: Twitter.StatusId -> m Twitter.Status

newtype GetStatusByIdT m a
  = GetStatusByIdT (m a)
  deriving newtype (Functor, Applicative, Monad)

instance (Monad m, Tw.MonadCall m) => MonadGetStatusById (GetStatusByIdT m) where
  getStatusById = GetStatusByIdT . Tw.call . Twitter.statusesShowId

newtype MockedGetStatusByIdT m a
  = MockedGetStatusByIdT { runMockedGetStatusByIdT :: m a }
  deriving newtype (Functor, Applicative, Monad)

instance Trans.MonadTrans MockedGetStatusByIdT where
  lift = MockedGetStatusByIdT

instance Monad m => MonadGetStatusById (MockedGetStatusByIdT m) where
  getStatusById _ = MockedGetStatusByIdT $ pure mockStatus

proceedIfNotEmpty
  :: forall subcontext context m
   . ( MonadSay m
     , MonadGetStatusById m
     , MonadReader context m
     , subcontext `GLens.Subtype` context
     , Tw.TargetTweetCount `GLens.HasType` subcontext
     )
  => ProceedIfNotEmptyArgs
  -> m ()
  -> m ()

proceedIfNotEmpty ProceedIfNotEmptyArgs{..} nextAction
  | not hasNext = do
      say "\nTwitter API returned no tweets for this request.\
          \ End of scraping.\n"

      targetTweetCount <-
        GLens.getTyped @Tw.TargetTweetCount . GLens.upcast @subcontext @context <$> ask

      when (show @_ @Text processedTweetCount /= show targetTweetCount)
        $ say "WARNING!"

      say
        $  "Target tweet count was " <> show targetTweetCount <> ".\n"
        <> "I processed " <> show processedTweetCount <> ".\n"

      Twitter.Status{..} <- getStatusById minimumId

      say $ "Oldest processed tweet was created at " <> show statusCreatedAt

  | otherwise = do
      say ""
      nextAction
