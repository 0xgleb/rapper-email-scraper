{-# LANGUAGE UndecidableInstances #-}

module TweetGetter.FreeSearch
  ( FreeSearch(..)
  )
  where

import TweetGetter.GetRapperTweets
import TweetGetter.SearchResult
import TwitterAuth
import User

import Protolude

import qualified Web.Twitter.Conduit                  as Twitter
import qualified Web.Twitter.Conduit.Request.Internal as Twitter
import qualified Web.Twitter.Conduit.Status           as Twitter
import qualified Web.Twitter.Types                    as Twitter

data FreeSearch
  = FreeSearch
      { maybeMaxId :: Maybe Twitter.StatusId
      }

instance (MonadIO m, HasTwitterAuth ctx m) => GetRapperTweets FreeSearch m where
  getRapperTweets = getFreeSearchTweets

getFreeSearchTweets
  :: ( HasTwitterAuth context m
     , MonadIO m
     , Monad m
     )
  => FreeSearch
  -> m (RequestResult m)

getFreeSearchTweets FreeSearch{..} = do
  statuses <- call query

  let nextRequest
        = fmap (getFreeSearchTweets . FreeSearch . Just)
        $ headMay $ sort $ id <$> tweets

      tweets = statuses <&> \Twitter.Status{..} ->
        Tweet
          { id        = statusId
          , truncated = statusTruncated
          , text      = statusText
          , user      = statusUser
          }

  pure RequestResult{..}

  where
    query = freeSearchQuery { Twitter._params = params }

    Twitter.APIRequest{..} = freeSearchQuery
    params = case maybeMaxId of
      Nothing    -> _params
      Just maxId -> ("max_id", Twitter.PVInteger maxId) : _params

freeSearchQuery :: Twitter.APIRequest Twitter.StatusesUserTimeline [Twitter.Status]
freeSearchQuery = query
  { Twitter._params = ("count", Twitter.PVInteger 200) : Twitter._params query }
  where
    query = Twitter.userTimeline sendBeatsBotHandle
