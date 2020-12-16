module Bot
  ( run
  )
  where

import           FileManager
import           Scraper
import qualified Twitter as Tw
import           Util

import Protolude
import Unsafe.Coerce

newtype Bot a
  = Bot { runBot :: ReaderT ScraperContext IO a }
  deriving MonadSay via IOSayT Bot
  deriving Tw.MonadCall via Tw.AuthorizedCallT Bot
  deriving MonadFileManager via IOFileManagerT Bot
  deriving MonadGetStatusById via GetStatusByIdT Bot
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader ScraperContext
    , MonadIO
    )

instance Tw.MonadRapperTweetsGetter Tw.FreeSearch Bot where
  getRapperTweets = unsafeCoerce
    $ (Tw.getRapperTweets
         :: Tw.FreeSearch
         -> Tw.FreeSearchT Bot
              (Tw.RequestResult (Tw.FreeSearchT Bot)))

instance Tw.MonadRapperTweetsGetter Tw.PremiumArchiveSearch Bot where
  getRapperTweets = unsafeCoerce
    $ (Tw.getRapperTweets
         :: Tw.PremiumArchiveSearch
         -> Tw.PremiumArchiveSearchT Bot
              (Tw.RequestResult (Tw.PremiumArchiveSearchT Bot)))

newtype TwitterMonad a
  = TwitterMonad { runTwitterMonad :: ReaderT Tw.Session IO a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Tw.Session)
  deriving MonadIO via ReaderT Tw.Session IO
  deriving MonadSay via IOSayT TwitterMonad
  deriving Tw.MonadCall via Tw.AuthorizedCallT TwitterMonad
  deriving Tw.MonadGetUser via Tw.GetUserT TwitterMonad

run :: IO ()
run = do
  session@Tw.PrivateSessionConstructor{..} <- Tw.createSession

  (userId, targetTweetCount) <-
    runReaderT (runTwitterMonad Tw.getUserData) session

  let mode = Free $ Tw.FreeSearch Nothing

  runReaderT (runBot (scrapeRapperEmails mode Nothing))
    ScraperContext{..}
