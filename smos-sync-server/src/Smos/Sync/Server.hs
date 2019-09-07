{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Smos.Sync.Server where

import GHC.Generics (Generic)

import Data.Aeson as JSON
import Data.ByteString (ByteString)
import Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import Data.UUID as UUID
import Data.UUID.V4 as UUID

import Text.Show.Pretty

import System.Exit

import Control.Concurrent.STM
import Control.Monad.Reader

import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp

import Servant

import Path
import Path.IO

import Data.Mergeful

import Smos.Sync.Server.OptParse

smosSyncServer :: IO ()
smosSyncServer = do
  Instructions dispatch Settings <- getInstructions
  case dispatch of
    DispatchServe ss -> serveSmosSyncServer ss

serveSmosSyncServer :: ServeSettings -> IO ()
serveSmosSyncServer ServeSettings{..} =  do
  serverStore <- readStore
  var <- newTVarIO serverStore
  Warp.run serveSetPort $ makeSyncApp $ ServerEnv {serverEnvStoreVar = var}

data SyncFile =
  SyncFile
    { syncFilePath :: Path Rel File
    , syncFileContents :: ByteString
    }
  deriving (Show, Eq, Generic)

instance FromJSON SyncFile where
  parseJSON =
    withObject "SyncFile" $ \o ->
      SyncFile <$> o .: "path" <*>
      (do base64Contents <- SB8.pack <$> o .: "contents"
          case Base64.decode base64Contents of
            Left err -> fail err
            Right r -> pure r)

instance ToJSON SyncFile where
  toJSON SyncFile {..} =
    object ["path" .= syncFilePath, "contents" .= SB8.unpack (Base64.encode syncFileContents)]

data ServerEnv =
  ServerEnv
    { serverEnvStoreVar :: TVar (ServerStore UUID SyncFile)
    }
  deriving (Generic)

makeSyncApp :: ServerEnv -> Wai.Application
makeSyncApp env =
  Servant.serve syncAPI $
  hoistServer syncAPI ((\func -> runReaderT func env) :: SyncHandler a -> Handler a) syncServer

syncAPI :: Proxy SyncAPI
syncAPI = Proxy

type SyncAPI
   = "sync" :> ReqBody '[ JSON] (SyncRequest UUID SyncFile) :> Post '[ JSON] (SyncResponse UUID SyncFile)

type SyncHandler = ReaderT ServerEnv Handler

syncServer :: ServerT SyncAPI SyncHandler
syncServer = handleSync

handleSync :: SyncRequest UUID SyncFile -> SyncHandler (SyncResponse UUID SyncFile)
handleSync request = do
  var <- asks serverEnvStoreVar
  store <- liftIO $ readTVarIO var
  liftIO $ pPrint store
  liftIO $ pPrint request
  (resp, newStore) <- processServerSync (liftIO UUID.nextRandom) store request
  liftIO $ do
    atomically $ writeTVar var newStore
    saveStore newStore
  liftIO $ pPrint newStore
  liftIO $ pPrint resp
  pure resp

readStore :: IO (ServerStore UUID SyncFile)
readStore = do
  mContents <- forgivingAbsence $ LB.readFile storeFile
  case mContents of
    Nothing -> pure initialServerStore
    Just contents ->
      case JSON.eitherDecode contents of
        Left err -> die err
        Right store -> pure store

saveStore :: ServerStore UUID SyncFile -> IO ()
saveStore = encodeFile storeFile

storeFile :: FilePath
storeFile = "store.json"
