module Scraper.Duplicates
  ( removeDuplicateEmails
  , removeDuplicates
  )
  where

import Protolude

removeDuplicateEmails :: Eq email => [(tweet, email)] -> [(tweet, email)]
removeDuplicateEmails
  = removeDuplicates snd

removeDuplicates :: Eq eq => (a -> eq) -> [a] -> [a]
removeDuplicates func = \case
  (x:xs) ->
    x : filter (\el -> func el /= func x) xs

  [] -> []
