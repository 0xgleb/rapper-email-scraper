module Bot
  ( run
  )
  where

import Scraper
import TwitterAuth
import User

import Protolude

run :: IO ()
run = do
  session <- createSession

  (userId, targetTweetCount) <- runReaderT getUserData session

  runReaderT scrapeRapperEmails ScraperContext{..}
