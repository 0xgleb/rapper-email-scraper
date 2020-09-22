module Bot
  ( run
  )
  where

import Scraper
import TwitterAuth

import Protolude

run :: IO ()
run = do
  twitterAuth <- getTwitterEnvVars

  scrapeRapperEmails twitterAuth