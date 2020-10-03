module User
  ( sendBeatsBotHandle

  , TargetTweetCount
  , getUserData
  )
  where

import TwitterAuth

import Protolude

import qualified Web.Twitter.Conduit as Twitter
import qualified Web.Twitter.Types   as Twitter

sendBeatsBotHandle :: Twitter.UserParam
sendBeatsBotHandle = Twitter.ScreenNameParam "SendBeatsBot"

newtype TargetTweetCount
  = TargetTweetCount Int
  deriving newtype (Show)

getUserData :: (MonadReader Session m, MonadIO m) => m TargetTweetCount
getUserData = do
  Twitter.User{..} <-
    call $ Twitter.usersShow sendBeatsBotHandle

  putStrLn @Text $ "SendBeatsBot has " <> show userStatusesCount <> " tweets\n\n"

  pure $ TargetTweetCount userStatusesCount
