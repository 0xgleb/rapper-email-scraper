module User
  ( sendBeatsBotHandle

  , TargetTweetCount
  , getUserData
  )
  where

import TwitterAuth

import Protolude

import qualified Data.Generics.Product as GLens
import qualified Web.Twitter.Conduit   as Twitter
import qualified Web.Twitter.Types     as Twitter

sendBeatsBotHandle :: Twitter.UserParam
sendBeatsBotHandle = Twitter.ScreenNameParam "SendBeatsBot"

newtype TargetTweetCount
  = TargetTweetCount Int
  deriving newtype (Show)

getUserData
  :: ( MonadReader context m
     , Session `GLens.HasType` context
     , MonadIO m
     )
  => m (Twitter.UserId, TargetTweetCount)

getUserData = do
  Twitter.User{..} <-
    call $ Twitter.usersShow sendBeatsBotHandle

  putStrLn @Text $ "SendBeatsBot has " <> show userStatusesCount <> " tweets\n\n"

  pure (userId, TargetTweetCount userStatusesCount)
