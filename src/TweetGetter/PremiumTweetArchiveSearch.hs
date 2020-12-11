{-# LANGUAGE UndecidableInstances #-}

module TweetGetter.PremiumTweetArchiveSearch
  ( PremiumTweetArchiveSearch(..)
  )
  where

import TweetGetter.GetRapperTweets
import TweetGetter.SearchResult
import TwitterAuth
import Util

import Protolude

import qualified Data.Time                            as Time
import qualified Web.Twitter.Conduit                  as Twitter
import qualified Web.Twitter.Conduit.Request.Internal as Twitter

data PremiumTweetArchiveSearch
  = PremiumTweetArchiveSearch
      { nextToken :: !(Maybe NextPageToken)
      , toDate    :: !(Maybe Time.UTCTime)
      }

instance
  (MonadIO m, HasTwitterAuth ctx m)
  => GetRapperTweets PremiumTweetArchiveSearch m where
  getRapperTweets = getPremiumSearchQuery

getPremiumSearchQuery
  :: ( HasTwitterAuth context m
     , MonadIO m
     , Monad m
     )
  => PremiumTweetArchiveSearch
  -> m (RequestResult m)

getPremiumSearchQuery PremiumTweetArchiveSearch{..} = do
  SearchResults{..} <- call query

  let nextRequest = case next of
        Nothing -> Nothing
        token@(Just _) ->
          Just $ getPremiumSearchQuery
            PremiumTweetArchiveSearch
              { nextToken = token
              , ..
              }

  pure RequestResult
    { tweets = results
    , ..
    }

  where
    query = premiumSearchQuery
      { Twitter._params = _params <> nextParam <> toDateParam }

    toDateParam = toList $ toDate <&> \date ->
      ("toDate", Twitter.PVString $ dayToTwitterTime date)

    nextParam = toList $ nextToken <&> \token ->
      ("next", Twitter.PVString $ getNextPageToken token)

    Twitter.APIRequest{..} = premiumSearchQuery


premiumSearchQuery :: Twitter.APIRequest Twitter.SearchTweets SearchResults
premiumSearchQuery
  = Twitter.APIRequest
      { _method = "GET"
      , _url    = "https://api.twitter.com/1.1/tweets/search/fullarchive/prod.json"
      , _params =
          [ ("query", Twitter.PVString "(from:SendBeatsBot)")
          , ("fromDate", Twitter.PVString "201607220000")
          , ("maxResults", Twitter.PVInteger 500)
          ]
      }
