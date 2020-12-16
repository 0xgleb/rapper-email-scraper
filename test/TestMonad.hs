{-# OPTIONS_GHC -Wno-orphans #-}

module TestMonad
  ( TestMonad
  , runTestMonad
  )
  where

import FileManager
import Scraper
import Util

import Protolude

import qualified Data.Map.Strict as Map

runTestMonad :: ctx -> TestMonad ctx a -> (a, [Text])
runTestMonad ctx
  = fst
  . flip runState (Map.fromList [])
  . runStateFileManager
  -- . fmap fst
  . flip runStateT []
  . runMockSayT
  . runMockedGetStatusByIdT
  . flip runReaderT ctx
  . runTestExtractEmailsFromTweets

newtype TestMonad ctx a
  = TestMonad
      { runTestExtractEmailsFromTweets
        :: ReaderT ctx (MockedGetStatusByIdT (MockSayT StateFileManager)) a
      }
  deriving newtype (Functor, Applicative, Monad, MonadReader ctx)

instance MonadGetStatusById (TestMonad ctx) where
  getStatusById = TestMonad . lift . getStatusById

instance MonadFileManager (TestMonad ctx) where
  doesFileExist = TestMonad . lift . lift . lift . doesFileExist
  readJSONFile  = TestMonad . lift . lift . lift . readJSONFile
  readEmails    = TestMonad . lift . lift . lift . readEmails

  writeJSONFile = (TestMonad . lift . lift . lift) ... writeJSONFile

  saveUnsavedEmails filePath
    = (TestMonad . lift . lift . lift) ... saveUnsavedEmails filePath

instance MonadSay (TestMonad ctx) where
  say = TestMonad . lift . lift . say
