{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Sync.API where

import GHC.Generics (Generic)

import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import Data.ByteString (ByteString)
import Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as SB8
import qualified Data.Text as T
import Data.UUID as UUID

import Control.Concurrent.STM
import Control.Monad.Reader

import Servant

import Path

import Data.Mergeful

instance FromJSONKey (Path Rel File) where
  fromJSONKey =
    FromJSONKeyTextParser $ \t ->
      case parseRelFile (T.unpack t) of
        Nothing -> fail "failed to parse relative file"
        Just rf -> pure rf

instance ToJSONKey (Path Rel File) where
  toJSONKey = toJSONKeyText $ T.pack . fromRelFile

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

syncAPI :: Proxy SyncAPI
syncAPI = Proxy

type SyncAPI
   = "sync" :> ReqBody '[ JSON] (SyncRequest UUID SyncFile) :> Post '[ JSON] (SyncResponse UUID SyncFile)

type SyncHandler = ReaderT ServerEnv Handler
