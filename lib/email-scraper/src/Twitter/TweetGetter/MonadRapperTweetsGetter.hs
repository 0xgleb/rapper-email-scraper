module Twitter.TweetGetter.MonadRapperTweetsGetter
  ( RequestResult(..)
  , MonadRapperTweetsGetter(..)
  )
  where

import Twitter.TweetGetter.SearchResult

import Protolude

data RequestResult m
  = RequestResult
      { tweets      :: ![Tweet]
      , nextRequest :: Maybe (m (RequestResult m))
      }

class Monad m => MonadRapperTweetsGetter (requestData :: Type) (m :: Type -> Type) where
  getRapperTweets :: requestData -> m (RequestResult m)
