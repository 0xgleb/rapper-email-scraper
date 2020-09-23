module Scraper
  ( scrapeRapperEmails
  )
  where

import Scraper.EmailFinder
import Scraper.MaxId
import TwitterAuth

import Protolude

import qualified Web.Twitter.Conduit as Twitter
import qualified Web.Twitter.Types   as Twitter


scrapeRapperEmails :: Session -> IO ()
scrapeRapperEmails session@Session{..} = do
  putStrLn @Text "Starting search #1"

  result@Twitter.SearchResult{..} <-
    Twitter.call twInfo manager searchQuery

  extractEmails $ Twitter.searchResultStatuses result

  let Twitter.APIRequest{..} = searchQuery

  scrapeNext session 2 searchResultSearchMetadata


scrapeNext :: Session -> Integer -> Twitter.SearchMetadata -> IO ()
scrapeNext session@Session{..} scrapeNum Twitter.SearchMetadata{..}
  = case searchMetadataNextResults of
      Nothing ->
        putStrLn @Text "Ran out of tweets"

      Just nextResults -> case extractMaxId nextResults of
        Nothing ->
          putStrLn @Text
            $  "Parameter 'max_id' not found in the search metadata:\n"
            <> show searchMetadataNextResults

        Just maxId -> do
          let Twitter.APIRequest{..} = searchQuery

              nextQuery
                = searchQuery { Twitter._params = ("max_id", maxId) : _params }

          putStrLn @Text $ "Starting search #" <> show scrapeNum

          result@Twitter.SearchResult{..} <-
            Twitter.call twInfo manager nextQuery

          extractEmails $ Twitter.searchResultStatuses result

          scrapeNext session (scrapeNum + 1) searchResultSearchMetadata


searchQuery :: Twitter.APIRequest Twitter.SearchTweets (Twitter.SearchResult [Twitter.Status])
searchQuery
  = Twitter.searchTweets "(from:SendBeatsBot) -filter:links -filter:replies"
