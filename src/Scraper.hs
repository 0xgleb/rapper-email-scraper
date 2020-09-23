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

  extractEmails statuses

  proceedIfNotEmpty statuses $ \Twitter.Status{..} ->
    scrapeNext session 2 (statusId - 1)


scrapeNext :: Session -> Int -> Twitter.StatusId -> IO ()
scrapeNext session@Session{..} count maxId = do
  putStrLn @Text
    $ "Starting request #" <> show count <> " with max_id=" <> show maxId

  statuses <- Twitter.call twInfo manager nextQuery

  extractEmails statuses

  putStrLn @Text "Request processing complete. Email search and extraction completed."

  proceedIfNotEmpty statuses $ \Twitter.Status{..} ->
    scrapeNext session (count + 1) (statusId - 1)

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
          = ("count", Twitter.PVInteger 2)
          : ("max_id", Twitter.PVInteger 1272624973031432191)
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
