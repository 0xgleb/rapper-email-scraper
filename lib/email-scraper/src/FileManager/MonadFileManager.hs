module FileManager.MonadFileManager
  ( FileIdentifier(..)
  , MonadFileManager(..)
  )
  where

import Protolude

import qualified Data.Aeson  as Aeson
import qualified Data.Coerce as Coerce

newtype FileIdentifier
  = FileIdentifier { getFileIdentifier :: FilePath }
  deriving newtype (Show)

class Monad m => MonadFileManager (m :: Type -> Type) where
  doesFileExist
    :: FileIdentifier -> m Bool

  readJSONFile
    :: Aeson.FromJSON a => FileIdentifier -> m (Maybe a)

  writeJSONFile
    :: Aeson.ToJSON a => FileIdentifier -> a -> m ()

  readEmails
    :: Coerce.Coercible Text a => FileIdentifier -> m [a]

  saveUnsavedEmails
    :: ( Coerce.Coercible a Text , Eq a )
    => FileIdentifier
    -> [a]
    -> [a]
    -> m [a]
