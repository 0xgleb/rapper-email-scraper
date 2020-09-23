module Scraper.Duplicates
  ( removeDuplicateEmails
  , removeDuplicates
  )
  where

import Protolude

removeDuplicateEmails :: Eq email => [(id, tweet, email)] -> [(id, tweet, email)]
removeDuplicateEmails
  = removeDuplicates $ \(_, _, email) -> email

removeDuplicates :: Eq eq => (a -> eq) -> [a] -> [a]
removeDuplicates func = \case
  (x:xs) ->
    x : filter (\el -> func el /= func x) xs

  [] -> []
