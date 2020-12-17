{-# LANGUAGE AllowAmbiguousTypes #-}

module Scraper.Email
  ( extractEmailsFromTweets

  -- * exported for testing purposes only
  , Email(..)
  , findEmailInText
  )
  where

import FileManager
import Scraper.Duplicates
import Scraper.Unmatched
import Twitter.TweetGetter.SearchResult
import Util

import Control.Lens
import Protolude
import Text.Regex.Posix (Regex)

import qualified Data.Aeson as Aeson
import qualified Data.Generics.Product as GLens
import qualified Data.Text             as Txt
import qualified Text.Regex.Lens       as Regex
import qualified Text.Regex.Quote      as Regex
import qualified Web.Twitter.Types     as Twitter


extractEmailsFromTweets
  :: forall subcontext context m
   . ( MonadFileManager m
     , MonadSay m
     , MonadReader context m
     , subcontext `GLens.Subtype` context
     , Twitter.UserId `GLens.HasType` subcontext
     )
  => [Tweet]
  -> m [Email]

extractEmailsFromTweets tweets = do
  userId <- GLens.getTyped @Twitter.UserId . GLens.upcast @subcontext <$> ask

  let filterFunc = (/= userId) . Twitter.userId . GLens.getTyped
  when (any filterFunc tweets)
    $ say
    $  "\nWARNING: "
    <> show (length $ filter filterFunc tweets)
    <> " fetched tweets don't match the user!\n"

  let maybeEmails
        = removeDuplicateEmails
        $ zipWith
            (\Tweet{..} email -> (id, text, truncated, email))
            tweets
        $ findEmailInText . (\Tweet{..} -> text) <$> tweets

      filePath = "rapper-emails.txt"

  savedEmails <- readEmails filePath

  let (emails, truncatedTweets)
        = flipFoldl ([], []) maybeEmails $ \accumulated -> \case
            (_, _, _, Just email) ->
              (email : fst accumulated, snd accumulated)

            (statusId, tweetText, truncated, Nothing) ->
              ( fst accumulated
              , if truncated
                then UnmatchedTweet{..} : snd accumulated
                else snd accumulated
              )

  saveUnmatchedTweets truncatedTweets

  saveUnsavedEmails filePath savedEmails
    $ reverse $ filter (not . (`elem` savedEmails)) emails


newtype Email
  = Email Text
  deriving newtype (Show, Eq, Aeson.ToJSON)

findEmailInText :: Text -> Maybe Email
findEmailInText text
  =   Email . Txt.toLower . decodeUtf8 . Regex._matchedString
  <$> encodeUtf8 text ^? Regex.regex emailRegex

  where
    emailRegex
      = [Regex.r|[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}|]
