module Twitter.User
  ( sendBeatsBotHandle

  , TargetTweetCount
  , getUserData
  )
  where

import Twitter.Call
import Util

import Protolude

import qualified Web.Twitter.Conduit as Twitter
import qualified Web.Twitter.Types   as Twitter

sendBeatsBotHandle :: Twitter.UserParam
sendBeatsBotHandle = Twitter.ScreenNameParam "SendBeatsBot"

newtype TargetTweetCount
  = TargetTweetCount Int
  deriving newtype (Show)

getUserData
  :: ( MonadCall m
     , MonadSay m
     )
  => m (Twitter.UserId, TargetTweetCount)

getUserData = do
  Twitter.User{..} <-
    call $ Twitter.usersShow sendBeatsBotHandle

  say $ "SendBeatsBot has " <> show userStatusesCount <> " tweets\n\n"

  pure (userId, TargetTweetCount userStatusesCount)
