module Util
  ( flipFoldl
  , dayToTwitterTime
  )
  where

import Protolude

import qualified Data.Text as Txt
import qualified Data.Time as Time

flipFoldl
  :: Foldable foldable
  => state
  -> foldable val
  -> (state -> val -> state)
  -> state

flipFoldl init foldable accumulator
  = foldl accumulator init foldable


dayToTwitterTime :: Time.UTCTime -> Text
dayToTwitterTime
  = (<> "0000") . Txt.filter (/= '-') . show . Time.utctDay
