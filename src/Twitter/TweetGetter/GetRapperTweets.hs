module Twitter.TweetGetter.GetRapperTweets
  ( RequestResult(..)
  , GetRapperTweets(..)
  )
  where

import Twitter.TweetGetter.SearchResult

import Protolude

data RequestResult m
  = RequestResult
      { tweets      :: ![Tweet]
      , nextRequest :: Maybe (m (RequestResult m))
      }

class GetRapperTweets (requestData :: Type) (m :: Type -> Type) where
  getRapperTweets :: requestData -> m (RequestResult m)
