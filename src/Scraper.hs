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
  putStrLn @Text "Starting initial request"

  statuses <-
    Twitter.call twInfo manager searchQuery

  extractEmails statuses -- $ Twitter.searchResultStatuses result

  case lastMay $ sortOn Twitter.statusId statuses of
    Just Twitter.Status{..} ->
      scrapeNext session 2 (statusId - 1)

    Nothing ->
      putStrLn @Text "headMay returned Nothing after first request"


scrapeNext :: Session -> Int -> Twitter.StatusId -> IO ()
scrapeNext session@Session{..} count maxId = do
  let Twitter.APIRequest{..} = searchQuery

      nextQuery = searchQuery
        { Twitter._params = ("max_id", Twitter.PVInteger maxId) : _params }

  putStrLn @Text
    $ "Starting search #" <> show count <> " with max_id=" <> show maxId

  statuses <- Twitter.call twInfo manager nextQuery

  extractEmails statuses

  putStrLn @Text "Search complete. Emails extracted.\n"

  case lastMay $ sortOn Twitter.statusId statuses of
    Just Twitter.Status{..} ->
      scrapeNext session (count + 1) (statusId - 1)

    Nothing ->
      putStrLn @Text "headMay returned Nothing"


searchQuery :: Twitter.APIRequest Twitter.StatusesUserTimeline [Twitter.Status]
searchQuery
  = let query = Twitter.userTimeline $ Twitter.ScreenNameParam botHandle

    in query { Twitter._params
                 = ("count", Twitter.PVInteger 2)
                 : ("max_id", Twitter.PVInteger 1294726112400965631)
                 : Twitter._params query
             }

  where
    botHandle = "SendBeatsBot"
