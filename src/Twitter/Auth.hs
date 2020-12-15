module Twitter.Auth
  ( Session(..)
  , createSession
  , HasTwitterAuth
  )
  where

import Protolude

import qualified Data.Default          as Default
import qualified Data.Generics.Product as Generic
import qualified System.Environment    as Env
import qualified Web.Twitter.Conduit   as Twitter


data Session
  = Session_
      { manager :: !Twitter.Manager
      , twInfo  :: !Twitter.TWInfo
      }

type HasTwitterAuth context m
  = ( MonadReader context m
    , Session `Generic.HasType` context
    )

createSession :: IO Session
createSession = do
  TwitterAuthentication{..} <- getTwitterEnvVars

  manager <-
    Twitter.newManager Twitter.tlsManagerSettings

  let twInfo = Twitter.setCredential oauth credential Default.def

      oauth = Twitter.twitterOAuth
        { Twitter.oauthConsumerKey = consumerKey
        , Twitter.oauthConsumerSecret = consumerSecret
        }

      credential = Twitter.Credential
        [ ("oauth_token", accessToken)
        , ("oauth_token_secret", accessSecret)
        ]

  pure Session_{..}


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
