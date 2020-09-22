module Lib
  ( scrapeRapperEmails
  , extractMaxId
  )
  where

import Protolude
import qualified Prelude

import qualified Data.Aeson                           as Aeson
import qualified Data.Default                         as Default
import qualified Data.Text                            as Txt
import qualified Web.Twitter.Conduit                  as Twitter
import qualified Web.Twitter.Conduit.Request.Internal as Twitter
import qualified Web.Twitter.Types                    as Twitter

scrapeRapperEmails :: IO ()
scrapeRapperEmails = do
  manager <-
    Twitter.newManager Twitter.tlsManagerSettings

  let twInfo = Twitter.setCredential oauth credential Default.def

      oauth = Twitter.twitterOAuth
        { Twitter.oauthConsumerKey = "<OAUTH_CONSUMER_KEY>"
        , Twitter.oauthConsumerSecret = "<OAUTH_CONSUMER_SECRET>"
        }

      credential = Twitter.Credential
        [ ("oauth_token", "<OAUTH_ACCESS_TOKEN>")
        , ( "oauth_token_secret", "<OAUTH_ACCESS_TOKEN_SECRET>"
          )
        ]

  let searchQuery@Twitter.APIRequest{..}
        = Twitter.searchTweets "(from:SendBeatsBot) -filter:links -filter:replies"

  firstRes@Twitter.SearchResult{..} <-
    Twitter.call twInfo manager searchQuery

  putStrLn $ Aeson.encode firstRes

  let Twitter.SearchMetadata{..} = searchResultSearchMetadata

      secondSearch :: Twitter.APIRequest Twitter.SearchTweets (Twitter.SearchResult [Twitter.Status])
      secondSearch = case searchMetadataNextResults of
        Just nextResults -> case extractMaxId nextResults of
          Just maxId ->
            searchQuery { Twitter._params = ("max_id", maxId) : _params }

          Nothing ->
            Prelude.error "max_id not found"

        Nothing ->
          Prelude.error "ran out of tweets"

  putStrLn @Text "Doing 2nd search"

  secondRes <-
    Twitter.call twInfo manager secondSearch

  putStrLn $ Aeson.encode secondRes

  pure ()

data BreakMode
  = BreakOnStart
  | BreakOnEnd

extractMaxId :: Twitter.URIString -> Maybe Twitter.PV
extractMaxId
  =   fmap Twitter.PVInteger . readMaybe . Txt.unpack
  <=< search BreakOnStart "&"
  <=< search BreakOnEnd "max_id="

  where
    search breakMode searchPart txt
      = let breakFunc = case breakMode of
              BreakOnStart -> Txt.breakOn
              BreakOnEnd   -> Txt.breakOnEnd

            (before, after) = breakFunc searchPart txt

        in case breakMode of
             BreakOnEnd ->
              if Txt.drop (Txt.length before - Txt.length searchPart) before == searchPart
              then Just after
              else Nothing

             BreakOnStart ->
              if searchPart == Txt.take (Txt.length searchPart) after
              then Just before
              else Nothing
