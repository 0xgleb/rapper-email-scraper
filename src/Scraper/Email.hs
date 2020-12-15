{-# LANGUAGE AllowAmbiguousTypes #-}

module Scraper.Email
  ( extractEmails

  -- * exported for testing purposes only
  , Email(..)
  , findEmailInText
  )
  where

import Scraper.Duplicates
import Scraper.Unmatched
import Twitter.TweetGetter.SearchResult
import Util

import Control.Lens
import Protolude
import Text.Regex.Posix (Regex)

import qualified Data.Generics.Product as GLens
import qualified Data.Text             as Txt
import qualified System.Directory      as Dir
import qualified Text.Regex.Lens       as Regex
import qualified Text.Regex.Quote      as Regex
import qualified Web.Twitter.Types     as Twitter


extractEmails
  :: ( MonadSay m
     , MonadIO m
     , MonadReader context m
     , Twitter.UserId `GLens.HasType` context
     )
  => [Tweet]
  -> m ()

extractEmails tweets = do
  userId <- GLens.getTyped @Twitter.UserId <$> ask

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

  fileExists <- liftIO $ Dir.doesFileExist filePath

  savedEmails <-
    if fileExists
    then liftIO $ fmap Email . Txt.lines <$> readFile filePath
    else pure []

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

  saveUnmatchedTweet truncatedTweets

  void $ forM (filter (not . flip elem savedEmails) emails) $ \(Email email) ->
    liftIO $ appendFile filePath $ email <> "\n"


newtype Email
  = Email Text
  deriving newtype (Show, Eq)

findEmailInText :: Text -> Maybe Email
findEmailInText text
  =   Email . Txt.toLower . decodeUtf8 . Regex._matchedString
  <$> encodeUtf8 text ^? Regex.regex emailRegex

  where
    emailRegex
      = [Regex.r|[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}|]
