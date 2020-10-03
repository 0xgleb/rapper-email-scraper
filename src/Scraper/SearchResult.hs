module Scraper.SearchResult
  ( SearchResults(..)
  , NextPageToken(..)
  , Result(..)
  , Retweet(..)

  , FlatTweet(..)
  , flattenResults
  )
  where

import Util

import Data.Aeson ((.:), (.:?))
import Protolude

import qualified Data.Aeson        as Aeson
import qualified Web.Twitter.Types as Twitter

newtype NextPageToken
  = NextPageToken { getNextPageToken :: Text }
  deriving newtype (Show, Aeson.FromJSON)

data SearchResults
  = SearchResults
      { next    :: Maybe NextPageToken
      , results :: [Result]
      }
  deriving stock (Generic, Show)

instance Aeson.FromJSON SearchResults where
  parseJSON = Aeson.genericParseJSON options

options :: Aeson.Options
options = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = identity
  , Aeson.constructorTagModifier = identity
  , Aeson.allNullaryToStringTag = False
  , Aeson.omitNothingFields = False
  , Aeson.sumEncoding = Aeson.TaggedObject
      { tagFieldName = "type"
      , contentsFieldName = "value"
      }
  , Aeson.unwrapUnaryRecords = False
  , Aeson.tagSingleConstructors = False
  }

data Result
  = Result
      { truncated       :: Bool
      , retweetedStatus :: Maybe Retweet
      }
  deriving stock (Generic, Show)

instance Aeson.FromJSON Result where
  parseJSON = Aeson.withObject "Coord" $ \v -> Result
    <$> v .: "truncated"
    <*> v .:? "retweeted_status"

data Retweet
  = Retweet
      { id   :: Integer
      , text :: Text
      , user :: Twitter.User
      }
  deriving stock (Generic, Show)

instance Aeson.FromJSON Retweet where
  parseJSON = Aeson.genericParseJSON options


data FlatTweet
  = FlatTweet
      { id        :: Twitter.StatusId
      , text      :: Text
      , user      :: Twitter.User
      , truncated :: Bool
      }
  deriving stock (Generic)

flattenResults :: [Result] -> [FlatTweet]
flattenResults results
  = flipFoldl [] results $ \accumulated Result{..} ->
      case retweetedStatus of
        Just Retweet{..} ->
          accumulated <> [FlatTweet{..}]

        Nothing ->
          accumulated
