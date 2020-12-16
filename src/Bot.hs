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
  deriving newtype (Functor, Applicative, Monad, MonadReader ScraperContext, MonadIO)
  deriving MonadSay via IOSayT (ReaderT ScraperContext IO)
  deriving Tw.MonadCall via Tw.AuthorizedCallT (ReaderT ScraperContext IO)
  deriving MonadFileManager via IOFileManagerT Bot

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

newtype TwitterT a
  = TwitterT { runTwitter :: ReaderT Tw.Session IO a }
  deriving newtype (Functor, Applicative, Monad)
  deriving MonadSay via (IOSayT (ReaderT Tw.Session IO))
  deriving Tw.MonadCall via (Tw.AuthorizedCallT (ReaderT Tw.Session IO))

run :: IO ()
run = do
  session@Tw.PrivateSessionConstructor{..} <- Tw.createSession

  (userId, targetTweetCount) <-
    runReaderT (runTwitter Tw.getUserData) session

  let mode = Free $ Tw.FreeSearch Nothing

  runReaderT (runBot (scrapeRapperEmails mode Nothing))
    ScraperContext{..}
