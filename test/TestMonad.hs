{-# OPTIONS_GHC -Wno-orphans #-}

module TestMonad
  ( TestMonad
  , runTestMonad
  , TestResult(..)
  )
  where

import FileManager
import Scraper
import Util
import qualified Twitter as Tw

import Protolude
import Unsafe.Coerce

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map

data TestResult a
  = TestResult
      { result     :: a
      , output     :: [Text]
      , fileSystem :: Map.Map FilePath BSL.ByteString
      }

tuplesToResult :: ((a, [Text]), Map FilePath BSL.ByteString) -> TestResult a
tuplesToResult ((result, output), fileSystem)
  = TestResult{..}

runTestMonad :: ctx -> TestMonad ctx a -> TestResult a
runTestMonad ctx
  = tuplesToResult
  . flip runState (Map.fromList [])
  . runStateFileManager
  -- . fmap fst
  . flip runStateT []
  . runMockSayT
  . runMockedGetStatusByIdT
  . Tw.runMockedCallT
  . flip runReaderT ctx
  . runTestExtractEmailsFromTweets

newtype TestMonad ctx a
  = TestMonad
      { runTestExtractEmailsFromTweets
          :: ReaderT ctx (Tw.MockedCallT (MockedGetStatusByIdT (MockSayT StateFileManager))) a
      }
  deriving newtype (Functor, Applicative, Monad, MonadReader ctx)

instance MonadGetStatusById (TestMonad ctx) where
  getStatusById = TestMonad . lift . lift . getStatusById

instance MonadFileManager (TestMonad ctx) where
  doesFileExist = TestMonad . lift . lift . lift . lift . doesFileExist
  readJSONFile  = TestMonad . lift . lift . lift . lift . readJSONFile
  readEmails    = TestMonad . lift . lift . lift . lift . readEmails

  writeJSONFile = (TestMonad . lift . lift . lift . lift) ... writeJSONFile

  saveUnsavedEmails filePath
    = (TestMonad . lift . lift . lift . lift) ... saveUnsavedEmails filePath

instance MonadSay (TestMonad ctx) where
  say = TestMonad . lift . lift . lift . say

instance Generic ctx => Tw.MonadRapperTweetsGetter Tw.FreeSearch (TestMonad ctx) where
  getRapperTweets = TestMonad . lift . unsafeCoerce
    . (Tw.getRapperTweets
         :: Tw.FreeSearch
         -> Tw.MockedCallT (MockedGetStatusByIdT (MockSayT StateFileManager))
              (Tw.RequestResult (Tw.MockedCallT (MockedGetStatusByIdT (MockSayT StateFileManager)))))

instance Generic ctx => Tw.MonadRapperTweetsGetter Tw.PremiumArchiveSearch (TestMonad ctx) where
  getRapperTweets = TestMonad . lift . unsafeCoerce
    . (Tw.getRapperTweets
         :: Tw.PremiumArchiveSearch
         -> Tw.MockedCallT (MockedGetStatusByIdT (MockSayT StateFileManager))
              (Tw.RequestResult (Tw.MockedCallT (MockedGetStatusByIdT (MockSayT StateFileManager)))))
