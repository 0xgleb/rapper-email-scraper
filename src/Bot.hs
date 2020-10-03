module Bot
  ( run
  )
  where

import Scraper
import TwitterAuth
import User

import Protolude

run :: IO ()
run = (createSession >>=) $ runReaderT $ do
  targetTweetCount <- getUserData

  scrapeRapperEmails targetTweetCount
