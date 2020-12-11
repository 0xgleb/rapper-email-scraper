{-# LANGUAGE UndecidableInstances #-}

module TweetGetter.SearchResult
  ( SearchResults(..)
  , NextPageToken(..)
  , Tweet(..)
  )
  where

import Protolude

import qualified Data.Aeson        as Aeson
import qualified Web.Twitter.Types as Twitter

newtype NextPageToken
  = NextPageToken { getNextPageToken :: Text }
  deriving newtype (Show, Aeson.FromJSON, Eq)

-- field names come from twitter. don't change them
data SearchResults
  = SearchResults
      { next    :: !(Maybe NextPageToken)
      , results :: ![Tweet]
      }
  deriving stock (Generic, Show, Eq)
  deriving Aeson.FromJSON via FromJSONT SearchResults

-- field names come from twitter. don't change them
data Tweet
  = Tweet
      { id        :: !Twitter.StatusId
      , truncated :: !Bool
      , text      :: !Text
      , user      :: !Twitter.User
      }
  deriving stock (Generic, Show, Eq)
  deriving Aeson.FromJSON via FromJSONT Tweet


options :: Aeson.Options
options = Aeson.defaultOptions
  { Aeson.fieldLabelModifier     = identity
  , Aeson.constructorTagModifier = identity
  , Aeson.allNullaryToStringTag  = False
  , Aeson.omitNothingFields      = False
  , Aeson.unwrapUnaryRecords     = False
  , Aeson.tagSingleConstructors  = False
  , Aeson.sumEncoding
      = Aeson.TaggedObject
          { tagFieldName = "type"
          , contentsFieldName = "value"
          }
  }

newtype FromJSONT val
  = FromJSONT val

instance
  ( Generic val
  , Aeson.GFromJSON Aeson.Zero (Rep val)
  ) => Aeson.FromJSON (FromJSONT val)
  where
    parseJSON = fmap FromJSONT . Aeson.genericParseJSON options
