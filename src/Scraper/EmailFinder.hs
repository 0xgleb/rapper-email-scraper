module Scraper.EmailFinder
  ( Email(..)
  , findEmailInText
  )
  where

import Control.Lens
import Protolude
import Text.Regex.Posix (Regex)

import qualified Data.Text as Txt
import qualified Text.Regex.Lens  as Regex
import qualified Text.Regex.Quote as Regex

newtype Email
  = Email { getEmail :: Text }
  deriving newtype (Show, Eq)

findEmailInText :: Text -> Maybe Email
findEmailInText text
  =   Email . Txt.toLower . decodeUtf8 . Regex._matchedString
  <$> encodeUtf8 text ^? Regex.regex emailRegex

  where
    emailRegex
      = [Regex.r|[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}|]
