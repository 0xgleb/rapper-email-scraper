module Bot
  ( run
  )
  where

import Scraper
import TweetGetter
import TwitterAuth
import User

import Protolude

run :: IO ()
run = do
  session <- createSession

  (userId, targetTweetCount) <- runReaderT getUserData session

  let mode = Free $ FreeSearch Nothing

  runReaderT (scrapeRapperEmails mode Nothing)
    ScraperContext{..}
