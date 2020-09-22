module Scraper
  ( scrapeRapperEmails
  )
  where

import TwitterAuth

import Protolude

import qualified Data.Aeson          as Aeson
import qualified Data.Default        as Default
import qualified Web.Twitter.Conduit as Twitter
import qualified Web.Twitter.Types   as Twitter


scrapeRapperEmails :: TwitterAuthentication -> IO ()
scrapeRapperEmails TwitterAuthentication{..} = do
  manager <-
    Twitter.newManager Twitter.tlsManagerSettings

  let twInfo = Twitter.setCredential oauth credential Default.def

      oauth = Twitter.twitterOAuth
        { Twitter.oauthConsumerKey = consumerKey
        , Twitter.oauthConsumerSecret = consumerSecret
        }

      credential = Twitter.Credential
        [ ("oauth_token", accessToken)
        , ("oauth_token_secret", accessSecret)
        ]

  let searchQuery@Twitter.APIRequest{..}
        = Twitter.searchTweets "(from:SendBeatsBot) -filter:links -filter:replies"

  firstRes@Twitter.SearchResult{..} <-
    Twitter.call twInfo manager searchQuery

  putStrLn $ Aeson.encode firstRes

  -- let Twitter.SearchMetadata{..} = searchResultSearchMetadata

  --     secondSearch :: Twitter.APIRequest Twitter.SearchTweets (Twitter.SearchResult [Twitter.Status])
  --     secondSearch = case searchMetadataNextResults of
  --       Just nextResults -> case extractMaxId nextResults of
  --         Just maxId ->
  --           searchQuery { Twitter._params = ("max_id", maxId) : _params }

  --         Nothing ->
  --           Prelude.error "max_id not found"

  --       Nothing ->
  --         Prelude.error "ran out of tweets"

  -- putStrLn @Text "Doing 2nd search"

  -- secondRes <-
  --   Twitter.call twInfo manager secondSearch

  -- putStrLn $ Aeson.encode secondRes

  pure ()
