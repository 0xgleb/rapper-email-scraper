{-# LANGUAGE UndecidableInstances #-}

module FileManager.IOFileManager
  ( IOFileManagerT(..)
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
import qualified Data.Text                    as Txt
import qualified Network.AWS                  as AWS
import qualified Network.AWS.Data.Body        as AWS.Body
import qualified Network.AWS.S3               as S3
import qualified System.Directory             as Dir

newtype IOFileManagerT (m :: Type -> Type) (a :: Type)
  = IOFileManagerT (m a)
  deriving newtype (Functor, Applicative, Monad)

deriving newtype instance MonadReader S3.BucketName m
  => MonadReader S3.BucketName (IOFileManagerT m)

runAWST
  :: forall m a
   . ( MonadIO m
     , Catch.MonadCatch m
     , Resource.MonadUnliftIO m
     )
  => AWST.AWST' AWS.Env (Resource.ResourceT m) a
  -> m a
runAWST action = do
  env <- awsEnv

  AWS.runResourceT
    . AWST.runAWST env
    . AWST.within region
    . AWST.reconfigure S3.s3
    $ action
  where
    awsEnv :: m AWST.Env
    awsEnv = AWST.newEnv AWST.Discover

    region :: AWST.Region
    region = AWST.Ireland

getObject
  :: ( MonadIO m
     , Catch.MonadCatch m
     , Resource.MonadUnliftIO m
     )
  => S3.BucketName
  -> S3.ObjectKey
  -> m (Maybe ByteString)
getObject bucketName objectKey = liftIO $ runAWST $ do
  let getObject = S3.getObject bucketName objectKey

  object <- AWS.send getObject

  lift $ Conduit.runConduit
    $ AWS.Body._streamBody (object ^. S3.gorsBody) .| Conduit.await

fileIdentifierToObjectKey :: FileIdentifier -> S3.ObjectKey
fileIdentifierToObjectKey = S3.ObjectKey . Txt.pack . getFileIdentifier

putObject
  :: ( MonadIO m
     , Catch.MonadCatch m
     , Resource.MonadUnliftIO m
     )
  => S3.BucketName
  -> S3.ObjectKey
  -> ByteString
  -> m ()
putObject bucketName objectKey bytestring = liftIO $ runAWST $ do
  let putObject
        = S3.putObject bucketName
            (ObjectKey $ getFileIdentifier $ fileIdentifierToObjectKey)
        $ Aeson.encode value

  object <- AWS.send putObject

  lift $ Conduit.runConduit
    $ AWS.Body._streamBody (object ^. S3.gorsBody) .| Conduit.await

instance
  ( MonadIO m
  , Generic context
  , MonadReader context m
  , S3.BucketName `GLens.HasType` context
  ) => MonadFileManager (IOFileManagerT m)
  where
    doesFileExist fileIdentifier = IOFileManagerT $ do
      bucketName <- GLens.getTyped @S3.BucketName <$> ask
      object <- getObject bucketName $ fileIdentifierToObjectKey fileIdentifier
      pure $ isJust object

    readJSONFile fileIdentifier = IOFileManagerT $ do
      bucketName <- GLens.getTyped <$> ask
      object <- getObject bucketName $ fileIdentifierToObjectKey fileIdentifier
      pure $ Aeson.decode =<< _ object

    writeJSONFile fileIdentifier value = IOFileManagerT $ do
      bucketName <- GLens.getTyped <$> ask
      getObject bucketName $ fileIdentifierToObjectKey fileIdentifier
      putObject bucketName

    readEmails fileIdentifier = IOFileManagerT $ liftIO $ do
      bucketName <- GLens.getTyped <$> ask
      object <- getObject bucketName $ fileIdentifierToObjectKey fileIdentifier
      pure $ maybe [] (fmap (Coerce.coerce . Txt.lines) . _) object

    saveUnsavedEmails fileIdentifier savedEmails emails = IOFileManagerT $ liftIO $ do
      bucketName <- GLens.getTyped <$> ask
      object <- getObject bucketName $ fileIdentifierToObjectKey fileIdentifier
      let newEmails = filter (not . flip elem savedEmails) emails
          updatedBS = (object <>) $ foldMap $ \email ->
            Coerce.coerce email <> "\n"



      pure $ savedEmails <> newEmails
