{-# LANGUAGE UndecidableInstances #-}

module Twitter.TweetGetter.PremiumArchiveSearch
  ( PremiumArchiveSearch(..)
  , PremiumArchiveSearchT(..)
  )
  where

import Twitter.Auth
import Twitter.Call
import Twitter.TweetGetter.Mock
import Twitter.TweetGetter.MonadRapperTweetsGetter
import Twitter.TweetGetter.SearchResult
import Util

import Protolude

import qualified Data.Time                            as Time
import qualified Web.Twitter.Conduit                  as Twitter
import qualified Web.Twitter.Conduit.Request.Internal as Twitter

data PremiumArchiveSearch
  = PremiumArchiveSearch
      { nextToken :: !(Maybe NextPageToken)
      , toDate    :: !(Maybe Time.UTCTime)
      }

newtype PremiumArchiveSearchT m a
  = PremiumArchiveSearchT (m a)
  deriving newtype (Functor, Applicative, Monad, MonadCall)

deriving newtype instance MonadReader ctx m => MonadReader ctx (PremiumArchiveSearchT m)

instance
  ( MonadCall m, HasTwitterAuth ctx m )
  => MonadRapperTweetsGetter PremiumArchiveSearch (PremiumArchiveSearchT m)
  where
    getRapperTweets = getPremiumSearchQuery

getPremiumSearchQuery
  :: ( HasTwitterAuth context m
     , MonadCall m
     )
  => PremiumArchiveSearch
  -> m (RequestResult m)

getPremiumSearchQuery PremiumArchiveSearch{..} = do
  SearchResults{..} <- call query

  let nextRequest = case next of
        Nothing -> Nothing
        token@(Just _) ->
          Just $ getPremiumSearchQuery
            PremiumArchiveSearch
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

instance
  Monad m
  => MonadRapperTweetsGetter PremiumArchiveSearch (MockedCallT m)
  where
    getRapperTweets _searchRequest
      = pure RequestResult
          { tweets      = mockTweets
          , nextRequest = Nothing -- Just $ getRapperTweets searchRequest
          }
