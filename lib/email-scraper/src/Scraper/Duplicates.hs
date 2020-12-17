module Scraper.Duplicates
  ( removeDuplicateEmails
  , removeDuplicates
  )
  where

import Protolude

removeDuplicateEmails
  :: Eq email
  => [(id, tweet, truncated, email)]
  -> [(id, tweet, truncated, email)]

removeDuplicateEmails
  = removeDuplicates $ \(_, _, _, email) -> email

removeDuplicates :: Eq eq => (a -> eq) -> [a] -> [a]
removeDuplicates func = \case
  (x:xs) ->
    x : removeDuplicates func (filter (\el -> func el /= func x) xs)

  [] -> []
