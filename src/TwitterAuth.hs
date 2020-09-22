module TwitterAuth
  ( TwitterAuthentication(..)
  , getTwitterEnvVars
  )
  where

import Protolude

import qualified System.Environment as Env

data TwitterAuthentication
  = TwitterAuthentication
      { consumerKey    :: ByteString
      , consumerSecret :: ByteString
      , accessToken    :: ByteString
      , accessSecret   :: ByteString
      }

getTwitterEnvVars :: IO TwitterAuthentication
getTwitterEnvVars = do
  let getEnv = fmap toS . Env.getEnv

  consumerKey    <- getEnv "OAUTH_CONSUMER_KEY"
  consumerSecret <- getEnv "OAUTH_CONSUMER_SECRET"
  accessToken    <- getEnv "OAUTH_ACCESS_TOKEN"
  accessSecret   <- getEnv "OAUTH_ACCESS_TOKEN_SECRET"

  pure TwitterAuthentication{..}
