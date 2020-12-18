module FileManager.StateFileManager
  ( StateFileManager
  )
  where

import FileManager.MonadFileManager

import Protolude    hiding (pass)
import Control.Lens
import Data.Conduit ((.|))

import qualified Control.Monad.Catch          as Catch
import qualified Control.Monad.Trans.AWS      as AWST
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Aeson                   as Aeson
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.Coerce                  as Coerce
import qualified Data.Conduit                 as Conduit
import qualified Data.Generics.Product        as GLens
import qualified Data.Map.Strict              as Map
import qualified Data.Text                    as Txt
import qualified Network.AWS                  as AWS
import qualified Network.AWS.Data.Body        as AWS.Body
import qualified Network.AWS.S3               as S3
import qualified System.Directory             as Dir

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
