{-# LANGUAGE UndecidableInstances #-}

module Twitter.TweetGetter.FreeSearch
  ( FreeSearch(..)
  , FreeSearchT(..)

  , MockedCallT(..)
  , mockTweets
  )
  where

import Twitter.Auth
import Twitter.Call
import Twitter.TweetGetter.Mock
import Twitter.TweetGetter.MonadRapperTweetsGetter
import Twitter.TweetGetter.SearchResult
import Twitter.User

import Protolude

import qualified Web.Twitter.Conduit                  as Twitter
import qualified Web.Twitter.Conduit.Request.Internal as Twitter
import qualified Web.Twitter.Conduit.Status           as Twitter
import qualified Web.Twitter.Types                    as Twitter

data FreeSearch
  = FreeSearch
      { maybeMaxId :: !(Maybe Twitter.StatusId)
      }

newtype FreeSearchT m a
  = FreeSearchT (m a)
  deriving newtype (Functor, Applicative, Monad, MonadCall)

deriving newtype instance MonadReader ctx m => MonadReader ctx (FreeSearchT m)

instance
  ( MonadCall m
  , HasTwitterAuth ctx m
  ) => MonadRapperTweetsGetter FreeSearch (FreeSearchT m)
  where
    getRapperTweets = getFreeSearchTweets

getFreeSearchTweets
  :: forall m context
   . ( HasTwitterAuth context m
     , MonadCall m
     )
  => FreeSearch
  -> m (RequestResult m)

getFreeSearchTweets FreeSearch{..} = do
  statuses <- call query

  let nextRequest
        = fmap (getFreeSearchTweets @m . FreeSearch . Just)
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


instance Monad m => MonadRapperTweetsGetter FreeSearch (MockedCallT m) where
  getRapperTweets _searchRequest
    = pure RequestResult
        { tweets      = mockTweets
        , nextRequest = Nothing -- Just $ getRapperTweets searchRequest
        }
