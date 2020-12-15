module Bot
  ( run
  )
  where

import           Scraper
import qualified Twitter as Tw
import           Util

import Protolude

data BotCtx
  = BotCtx
      { scraperContext :: ScraperContext
      , session        :: Tw.Session
      }
  deriving stock (Generic)

newtype Bot m a
  = Bot { runBot :: ReaderT BotCtx IO a }
  deriving newtype (Functor, Applicative, Monad)
  deriving MonadSay via (IOSayT (ReaderT BotCtx IO))
  deriving Tw.MonadCall via (Tw.AuthorizedCallT (ReaderT BotCtx IO))

newtype TwitterT m a
  = TwitterT { runTwitter :: ReaderT Tw.Session IO a }
  deriving newtype (Functor, Applicative, Monad)
  deriving MonadSay via (IOSayT (ReaderT Tw.Session IO))
  deriving Tw.MonadCall via (Tw.AuthorizedCallT (ReaderT Tw.Session IO))

run :: IO ()
run = do
  session <- Tw.createSession

  (userId, targetTweetCount) <-
    runReaderT (runTwitter Tw.getUserData) session

  let mode = Free $ Tw.FreeSearch Nothing

  runReaderT (runBot (scrapeRapperEmails mode Nothing))
    BotCtx
      { scraperContext = ScraperContext{..}
      , ..
      }
