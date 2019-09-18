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
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Path ()
import Data.Validity.UUID ()

import Servant

import Path

import qualified Data.Mergeful as Mergeful

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

instance Validity SyncFile

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

syncAPI :: Proxy SyncAPI
syncAPI = Proxy

type SyncRequest = Mergeful.SyncRequest UUID SyncFile

data SyncResponse =
  SyncResponse
    { syncResponseServerId :: UUID
    , syncResponseItems :: Mergeful.SyncResponse UUID SyncFile
    }
  deriving (Show, Eq, Generic)

instance Validity SyncResponse

instance FromJSON SyncResponse where
  parseJSON = withObject "SyncResponse" $ \o -> SyncResponse <$> o .: "server-id" <*> o .: "items"

instance ToJSON SyncResponse where
  toJSON SyncResponse {..} =
    object ["server-id" .= syncResponseServerId, "items" .= syncResponseItems]

type SyncAPI = "sync" :> ReqBody '[ JSON] SyncRequest :> Post '[ JSON] SyncResponse
