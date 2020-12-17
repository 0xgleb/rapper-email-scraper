module FileManager
  ( MonadFileManager(..)
  , IOFileManagerT(..)
  , StateFileManager(..)
  )
  where

import Protolude hiding (pass)

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Coerce          as Coerce
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as Txt
import qualified System.Directory     as Dir

class Monad m => MonadFileManager (m :: Type -> Type) where
  doesFileExist
    :: FilePath -> m Bool

  readJSONFile
    :: Aeson.FromJSON a => FilePath -> m (Maybe a)

  writeJSONFile
    :: Aeson.ToJSON a => FilePath -> a -> m ()

  readEmails
    :: Coerce.Coercible Text a => FilePath -> m [a]

  saveUnsavedEmails
    :: ( Coerce.Coercible a Text , Eq a )
    => FilePath
    -> [a]
    -> [a]
    -> m [a]

newtype IOFileManagerT (m :: Type -> Type) (a :: Type)
  = IOFileManagerT (m a)
  deriving newtype (Functor, Applicative, Monad)

instance MonadIO m => MonadFileManager (IOFileManagerT m) where
  doesFileExist = IOFileManagerT . liftIO . Dir.doesFileExist

  readJSONFile
    = IOFileManagerT . liftIO . fmap Aeson.decode . BSL.readFile

  writeJSONFile filePath
    = IOFileManagerT . liftIO . BSL.writeFile filePath . Aeson.encode

  readEmails filePath = IOFileManagerT $ liftIO $ do
    fileExists <- Dir.doesFileExist filePath

    if fileExists
       then (Coerce.coerce . Txt.lines) <$> readFile filePath
       else pure []

  saveUnsavedEmails filePath savedEmails emails = IOFileManagerT $ liftIO $ do
    let newEmails = filter (not . flip elem savedEmails) emails
    void $ forM newEmails $ \email ->
      appendFile filePath $ Coerce.coerce email <> "\n"

    pure $ savedEmails <> newEmails


newtype StateFileManager a
  = StateFileManager
      { runStateFileManager :: State (Map.Map FilePath BSL.ByteString) a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState (Map.Map FilePath BSL.ByteString)
    )

instance MonadFileManager StateFileManager where
  doesFileExist filePath
    = StateFileManager $ (filePath `Map.member`) <$> get

  readJSONFile filePath
    = StateFileManager $ (Aeson.decode <=< Map.lookup filePath) <$> get

  writeJSONFile filePath value
    = StateFileManager $ state
    $ ((),) . Map.insert filePath (Aeson.encode value)

  readEmails filePath = StateFileManager $ do
    fileExists <- (filePath `Map.member`) <$> get

    if fileExists
       then readWriterEmails filePath
       else pure []

  saveUnsavedEmails filePath _ emails = StateFileManager $ do
    savedEmails <- readWriterEmails filePath
    let allEmails = savedEmails <> filter (not . (`elem` savedEmails)) emails
        text = foldl (\accum email -> accum <> Coerce.coerce email <> "\n")
                 ("" :: Text)
                 allEmails
    state $ ((),) . Map.insert filePath (toS text)
    pure allEmails

readWriterEmails
  :: Coerce.Coercible Text a
  => FilePath
  -> State (Map.Map FilePath BSL.ByteString) [a]
readWriterEmails filePath =
  ( maybe [] (Coerce.coerce . Txt.lines)
  . Map.lookup filePath
  . fmap toS
  ) <$> get
