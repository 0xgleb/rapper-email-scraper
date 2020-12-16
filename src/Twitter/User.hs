module Twitter.User
  ( sendBeatsBotHandle
  , TargetTweetCount(..)
  , MonadGetUser(..)
  , GetUserT(..)
  )
  where

import Twitter.Call
import Twitter.TweetGetter.Mock
import Util

import Protolude

import qualified Web.Twitter.Conduit as Twitter
import qualified Web.Twitter.Types   as Twitter

newtype TargetTweetCount
  = TargetTweetCount Int
  deriving newtype (Show)

class Monad m => MonadGetUser (m :: Type -> Type) where
  getUserData :: m (Twitter.UserId, TargetTweetCount)

newtype GetUserT m a
  = GetUserT (m a)
  deriving newtype (Functor, Applicative, Monad)

instance (MonadCall m, MonadSay m) => MonadGetUser (GetUserT m) where
  getUserData = GetUserT $ do
    Twitter.User{..} <-
      call $ Twitter.usersShow sendBeatsBotHandle

    say $ "SendBeatsBot has " <> show userStatusesCount <> " tweets\n\n"

    pure (userId, TargetTweetCount userStatusesCount)

instance Monad m => MonadGetUser (MockedCallT m) where
  getUserData = MockedCallT $ pure
    ( Twitter.userId mockUser
    , TargetTweetCount $ length mockTweets
    )

sendBeatsBotHandle :: Twitter.UserParam
sendBeatsBotHandle = Twitter.ScreenNameParam "SendBeatsBot"
