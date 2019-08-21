{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Smos.Server
  ( smosServer
  ) where

import GHC.Generics (Generic)

import Data.Aeson as JSON
import qualified Data.ByteString as SB
import Data.ByteString (ByteString)
import Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import Data.UUID as UUID
import Data.UUID.V4 as UUID

import System.Exit

import Control.Concurrent.STM
import Control.Monad.Reader

import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp

import Servant

import Path
import Path.IO

import Data.Mergeful

smosServer :: IO ()
smosServer = do
  mContents <- forgivingAbsence $ LB.readFile "store.json"
  serverStore <-
    case mContents of
      Nothing -> pure emptyServerStore
      Just contents ->
        case JSON.eitherDecode contents of
          Left err -> die err
          Right store -> pure store
  var <- newTVarIO serverStore
  Warp.run 8000 $ makeSyncApp $ ServerEnv {serverEnvStoreVar = var}

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
    object ["path" .= syncFilePath, "contents" .= SB.unpack (Base64.encode syncFileContents)]

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
  (resp, newStore) <- processServerSync (liftIO UUID.nextRandom) store request
  liftIO $ atomically $ writeTVar var newStore
  pure resp
