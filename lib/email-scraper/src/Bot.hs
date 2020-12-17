module Bot
  ( run
  )
  where

import qualified Twitter as Tw

import FileManager
import Scraper
import Util

import Protolude
import Unsafe.Coerce

import qualified Web.Twitter.Conduit   as Twitter
import qualified Web.Twitter.Types     as Twitter

data BotContext
  = BotContext
      { manager          :: !Twitter.Manager
      , twInfo           :: !Twitter.TWInfo
      , userId           :: !Twitter.UserId
      , targetTweetCount :: !Tw.TargetTweetCount
      }
  deriving stock (Generic)

newtype Bot a
  = Bot { runBot :: StateT [Email] (ReaderT BotContext IO) a }
  deriving MonadSay via IOSayT Bot
  deriving Tw.MonadCall via Tw.AuthorizedCallT Bot
  deriving MonadFileManager via IOFileManagerT Bot
  deriving MonadGetStatusById via GetStatusByIdT Bot
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader BotContext
    , MonadIO
    , MonadState [Email]
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

run :: MonadIO m => Mode -> m [Email]
run mode = liftIO $ do
  session@Tw.PrivateSessionConstructor{..} <- Tw.createSession

  (userId, targetTweetCount) <-
    runReaderT (runTwitterMonad Tw.getUserData) session

  fst <$> runReaderT (runStateT (runBot $ scrapeRapperEmails mode Nothing) [])
    BotContext{..}
