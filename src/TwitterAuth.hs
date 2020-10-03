module TwitterAuth
  ( Session
  , createSession
  , call
  )
  where

import Protolude

import qualified Control.Exception        as Exception
import qualified Data.Default             as Default
import qualified Data.Generics.Product    as G.P
import qualified System.Environment       as Env
import qualified System.Time.Extra        as Extra
import qualified Web.Twitter.Conduit      as Twitter
import qualified Web.Twitter.Conduit.Base as Twitter


data Session
  = Session
      { manager :: !Twitter.Manager
      , twInfo  :: !Twitter.TWInfo
      }

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

  pure Session{..}


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

call
  :: ( MonadReader context m
     , Session `G.P.HasType` context
     , MonadIO m
     , Twitter.ResponseBodyType responseType
     )
  => Twitter.APIRequest apiName responseType
  -> m responseType

call query = do
  Session{..} <- G.P.getTyped <$> ask

  let authedCall = Twitter.call twInfo manager query

  liftIO $ Exception.catch authedCall $ \(exception :: Exception.IOException) -> do
    print exception

    putStrLn @Text
      "I'm guessing that the Twitter rate limit was hit. \
      \I'll wait for 15 minutes and try again."

    Extra.sleep $ 15 * 60

    putStrLn @Text "Trying to perform the call again"

    authedCall
