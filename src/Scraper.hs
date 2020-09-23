module Scraper
  ( scrapeRapperEmails
  )
  where

import Scraper.EmailFinder
import TwitterAuth

import Protolude

import qualified Web.Twitter.Conduit                  as Twitter
import qualified Web.Twitter.Conduit.Request.Internal as Twitter
import qualified Web.Twitter.Conduit.Status           as Twitter
import qualified Web.Twitter.Types                    as Twitter


scrapeRapperEmails :: Session -> IO ()
scrapeRapperEmails session@Session{..} = do
  putStrLn @Text "Starting request #1"

  statuses <-
    Twitter.call twInfo manager searchQuery

  putStrLn @Text $ "Received " <> show (length statuses) <> " tweets"

  extractEmails statuses

  proceedIfNotEmpty statuses $ \Twitter.Status{..} ->
    scrapeNext session ScrapeNextRequest
      { requestCount = RequestCount 2
      , tweetCount   = TotalTweetCount $ toInteger $ length statuses
      , maxId        = statusId - 1
      }

newtype TotalTweetCount
  = TotalTweetCount Integer
  deriving newtype (Show, Num)

newtype RequestCount
  = RequestCount Integer
  deriving newtype (Show, Num)

data ScrapeNextRequest
  = ScrapeNextRequest
      { requestCount :: RequestCount
      , tweetCount   :: TotalTweetCount
      , maxId        :: Twitter.StatusId
      }

scrapeNext :: Session -> ScrapeNextRequest -> IO ()
scrapeNext session@Session{..} ScrapeNextRequest{..} = do
  putStrLn @Text
    $  "Starting request #" <> show requestCount
    <> " with max_id=" <> show maxId

  statuses <- Twitter.call twInfo manager nextQuery

  let currentTweetCount = TotalTweetCount (toInteger $ length statuses)
      totalTweetCount = tweetCount + currentTweetCount
  putStrLn @Text $ "Received " <> show currentTweetCount <> " tweets"

  extractEmails statuses

  putStrLn @Text "Request processing complete. Email search and extraction completed."
  putStrLn @Text $ "I have processed " <> show totalTweetCount <> " tweets in total"


  proceedIfNotEmpty statuses $ \Twitter.Status{..} ->
    scrapeNext session ScrapeNextRequest
      { requestCount = requestCount + 1
      , tweetCount   = totalTweetCount
      , maxId        = statusId - 1
      }


  where
    Twitter.APIRequest{..} = searchQuery

    nextQuery
      = searchQuery
          { Twitter._params
              = ("max_id", Twitter.PVInteger maxId)
              : _params
          }


searchQuery :: Twitter.APIRequest Twitter.StatusesUserTimeline [Twitter.Status]
searchQuery
  = query
      { Twitter._params
          = ("count", Twitter.PVInteger 200)
          : Twitter._params query
      }

  where
    query = Twitter.userTimeline $ Twitter.ScreenNameParam botHandle

    botHandle = "SendBeatsBot"


proceedIfNotEmpty
  :: [Twitter.Status]
  -> (Twitter.Status -> IO ())
  -> IO ()

proceedIfNotEmpty statuses nextAction
  = case lastMay $ sortOn Twitter.statusId statuses of
      Nothing ->
        putStrLn @Text "Twitter API returned no tweets for this request. End of scraping."

      Just status -> do
        putStrLn @Text ""
        nextAction status
