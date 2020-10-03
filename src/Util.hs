module Util
  ( flipFoldl
  )
  where

import Protolude

flipFoldl
  :: Foldable foldable
  => state
  -> foldable val
  -> (state -> val -> state)
  -> state

flipFoldl init foldable accumulator
  = foldl accumulator init foldable
