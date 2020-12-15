{-# LANGUAGE UndecidableInstances #-}

module Twitter.Call
  ( MonadCall(..)
  , AuthorizedCallT(..)
  )
  where

import Twitter.Auth

import Protolude

import qualified Control.Exception        as Exception
import qualified Data.Generics.Product    as G.P
import qualified System.Time.Extra        as Extra
import qualified Web.Twitter.Conduit      as Twitter
import qualified Web.Twitter.Conduit.Base as Twitter

class Monad m => MonadCall m where
  call
    :: forall apiName response
     . Twitter.ResponseBodyType response
    => Twitter.APIRequest apiName response
    -> m response

newtype AuthorizedCallT m a
  = AuthorizedCallT (m a)
  deriving newtype (Functor, Applicative, Monad)

instance
  ( Monad m
  , MonadIO m
  , HasTwitterAuth ctx m
  ) => MonadCall (AuthorizedCallT m)
  where
    call = AuthorizedCallT . authorizedCall

authorizedCall
  :: forall m context response apiName
   . ( HasTwitterAuth context m
     , Twitter.ResponseBodyType response
     , MonadIO m
     )
  => Twitter.APIRequest apiName response
  -> m response

authorizedCall query = do
  Session_{..} <- G.P.getTyped <$> ask

  let authedCall = Twitter.call twInfo manager query

  liftIO $ Exception.catch authedCall $ \(exception :: Exception.IOException) -> do
    print exception

    putStrLn @Text
      "I'm guessing that the Twitter rate limit was hit. \
      \I'll wait for 15 minutes and try again."

    Extra.sleep 60

    putStrLn @Text "Trying to perform the call again"

    authedCall
