{-# OPTIONS_GHC -Wno-orphans #-}

module TestMonad
  ( TestMonad
  , runTestMonad
  )
  where

import FileManager
import Util

import Protolude

import qualified Data.Map.Strict as Map

runTestMonad :: ctx -> TestMonad ctx a -> a
runTestMonad ctx
  = fst
  . flip runState (Map.fromList [])
  . runStateFileManager
  . fmap fst
  . flip runStateT []
  . runMockSayT
  . flip runReaderT ctx
  . runTestExtractEmailsFromTweets

newtype TestMonad ctx a
  = TestMonad
      { runTestExtractEmailsFromTweets
          :: ReaderT ctx (MockSayT StateFileManager) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader ctx)

instance MonadFileManager (TestMonad ctx) where
  doesFileExist = TestMonad . lift . doesFileExist
  readJSONFile  = TestMonad . lift . readJSONFile
  readEmails    = TestMonad . lift . readEmails

  writeJSONFile = (TestMonad . lift) ... writeJSONFile

  saveUnsavedEmails filePath
    = (TestMonad . lift) ... saveUnsavedEmails filePath

instance MonadSay (TestMonad ctx) where
  say = TestMonad . lift . say

instance MonadFileManager (MockSayT StateFileManager) where
  doesFileExist = MockSayT . lift . doesFileExist
  readJSONFile  = MockSayT . lift . readJSONFile
  readEmails    = MockSayT . lift . readEmails

  writeJSONFile = (MockSayT . lift) ... writeJSONFile

  saveUnsavedEmails filePath
    = (MockSayT . lift) ... saveUnsavedEmails filePath
