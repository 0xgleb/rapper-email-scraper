module Scraper.EmailFinder
  ( extractEmails

  -- * exported for testing purposes only
  , Email(..)
  , findEmailInText
  )
  where

import Scraper.Duplicates

import Control.Lens
import Protolude
import Text.Regex.Posix (Regex)

import qualified Data.Text         as Txt
import qualified System.Directory  as Dir
import qualified Text.Regex.Lens   as Regex
import qualified Text.Regex.Quote  as Regex
import qualified Web.Twitter.Types as Twitter


extractEmails :: [Twitter.Status] -> IO ()
extractEmails feed = do
  let tweets = Twitter.statusText <$> feed

      maybeEmails
        = removeDuplicateEmails
        $ zip tweets $ findEmailInText <$> tweets

      filePath = "rapper-emails.txt"

  fileExists <- Dir.doesFileExist filePath

  savedEmails <-
    if fileExists
    then fmap Email . Txt.lines <$> readFile filePath
    else pure []

  emails <-
    (\accumulator -> foldM accumulator [] maybeEmails) $ \accumulated -> \case
      (_, Just email) ->
        pure $ email : accumulated

      (tweet, Nothing) -> do
        putStrLn $ "Found a tweet with a missing email:\n" <> tweet
        pure accumulated


  void $ forM (filter (not . flip elem savedEmails) emails) $ \(Email email) ->
    appendFile filePath $ email <> "\n"


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
