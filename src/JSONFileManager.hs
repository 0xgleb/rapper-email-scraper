module JSONFileManager
  ( MonadJSONFileManager(..)
  , IOJSONFileManagerT(..)
  , StateJSONFileManager(..)
  )
  where

import           Protolude hiding (pass)

import qualified Data.ByteString.Lazy as BSL
import qualified System.Directory     as Dir
import qualified Data.Aeson           as Aeson
import qualified Data.Map.Strict as Map

class Monad m => MonadJSONFileManager (m :: Type -> Type) where
  doesFileExist
    :: FilePath -> m Bool

  writeJSONFile
    :: Aeson.ToJSON a => FilePath -> a -> m ()

  readJSONFile
    :: Aeson.FromJSON a => FilePath -> m (Maybe a)

newtype IOJSONFileManagerT (m :: Type -> Type) (a :: Type)
  = IOJSONFileManagerT (m a)
  deriving newtype (Functor, Applicative, Monad)

instance MonadIO m => MonadJSONFileManager (IOJSONFileManagerT m) where
  doesFileExist = IOJSONFileManagerT . liftIO . Dir.doesFileExist

  writeJSONFile filePath
    = IOJSONFileManagerT . liftIO . BSL.writeFile filePath . Aeson.encode

  readJSONFile
    = IOJSONFileManagerT . liftIO . fmap Aeson.decode . BSL.readFile

newtype StateJSONFileManager a
  = StateJSONFileManager
      { runStateJSONFileManager :: State (Map.Map FilePath BSL.ByteString) a }
  deriving newtype (Functor, Applicative, Monad, MonadState (Map.Map FilePath BSL.ByteString))

instance MonadJSONFileManager StateJSONFileManager where
  doesFileExist filePath
    = StateJSONFileManager $ (\files -> filePath `Map.member` files) <$> get

  writeJSONFile filePath value
    = StateJSONFileManager
    $ state (\files -> ((), Map.insert filePath (Aeson.encode value) files))

  readJSONFile filePath
    = StateJSONFileManager
    $ fmap (Aeson.decode <=< Map.lookup filePath) get
