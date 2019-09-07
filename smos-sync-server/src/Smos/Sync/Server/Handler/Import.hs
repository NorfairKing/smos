{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Sync.Server.Handler.Import
  ( module X
  , module Smos.Sync.Server.Handler.Import
  ) where

import GHC.Generics (Generic)

import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.UUID as X
import Data.UUID.V4 as UUID

import Text.Show.Pretty as X

import Path

import System.Exit

import Control.Concurrent.STM
import Control.Monad.Reader

import qualified Data.Mergeful as Mergeful

import Servant

import Path.IO

import Smos.Sync.API as X

type SyncHandler = ReaderT ServerEnv Handler

data ServerEnv =
  ServerEnv
    { serverEnvServerUUID :: UUID
    , serverEnvStoreFile :: Path Abs File
    , serverEnvStoreVar :: TVar (Mergeful.ServerStore UUID SyncFile)
    }
  deriving (Generic)

data ServerStore =
  ServerStore
    { serverStoreServerUUID :: UUID
    , serverStoreItems :: Mergeful.ServerStore UUID SyncFile
    }
  deriving (Show, Eq, Generic)

instance FromJSON ServerStore where
  parseJSON = withObject "ServerStore" $ \o -> ServerStore <$> o .: "server-id" <*> o .: "items"

instance ToJSON ServerStore where
  toJSON ServerStore {..} =
    object ["server-id" .= serverStoreServerUUID, "items" .= serverStoreItems]

readStore :: Path Abs File -> IO ServerStore
readStore p = do
  mContents <- forgivingAbsence $ LB.readFile $ fromAbsFile p
  case mContents of
    Nothing -> do
      u <- UUID.nextRandom
      pure ServerStore {serverStoreServerUUID = u, serverStoreItems = Mergeful.initialServerStore}
    Just contents ->
      case JSON.eitherDecode contents of
        Left err -> die err
        Right store -> pure store

saveStore :: Path Abs File -> ServerStore -> IO ()
saveStore p = encodeFile (fromAbsFile p)
