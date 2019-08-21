{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Smos.Sync.Client
  ( smosSyncClient
  ) where

import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.UUID as UUID

import System.Exit

import Servant.Client

import Path
import Path.IO

import Data.Mergeful

import Network.HTTP.Client as HTTP

import Smos.Sync.Server

smosSyncClient :: IO ()
smosSyncClient = do
  clientStore <- readClientStore
  man <- HTTP.newManager HTTP.defaultManagerSettings
  burl <- parseBaseUrl "localhost:8000"
  let cenv = mkClientEnv man burl
  errOrResp <- runClient cenv $ clientSync $ makeSyncRequest clientStore
  resp <-
    case errOrResp of
      Left err -> die $ show err
      Right resp -> pure resp
  saveClientStore $ mergeSyncResponseIgnoreProblems clientStore resp

runClient :: ClientEnv -> ClientM a -> IO (Either ServantError a)
runClient = flip runClientM

clientSync :: SyncRequest UUID SyncFile -> ClientM (SyncResponse UUID SyncFile)
clientSync = client syncAPI

clientStoreFile :: FilePath
clientStoreFile = "client-store.json"

readClientStore :: IO (ClientStore UUID SyncFile)
readClientStore = do
  mContents <- forgivingAbsence $ LB.readFile clientStoreFile
  case mContents of
    Nothing -> pure emptyClientStore
    Just contents ->
      case JSON.eitherDecode contents of
        Left err -> die err
        Right store -> pure store

saveClientStore :: ClientStore UUID SyncFile -> IO ()
saveClientStore = encodeFile clientStoreFile
