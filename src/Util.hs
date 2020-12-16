module Util
  ( flipFoldl
  , dayToTwitterTime
  , (...)
  , MonadSay(..)
  , IOSayT(..)
  , MockSayT(..)
  )
  where

import Protolude

import qualified Data.Text as Txt
import qualified Data.Time as Time
import qualified Control.Monad.Trans as Trans

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

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.).(.)

class Monad m => MonadSay (m :: Type -> Type) where
  say :: Text -> m ()

newtype IOSayT m a
  = IOSayT (m a)
  deriving newtype (Functor, Applicative, Monad)

instance MonadIO m => MonadSay (IOSayT m) where
  say = IOSayT . putStrLn -- TODO: add time

newtype MockSayT m a
  = MockSayT { runMockSayT :: StateT [Text] m a }
  deriving newtype (Functor, Applicative, Monad)

instance Trans.MonadTrans MockSayT where
  lift = MockSayT . lift

instance Monad m => MonadSay (MockSayT m) where
  say text = MockSayT $ state $ ((),) . (<> [text])
