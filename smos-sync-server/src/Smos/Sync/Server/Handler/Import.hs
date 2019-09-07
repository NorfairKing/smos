{-# LANGUAGE DeriveGeneric #-}

module Smos.Sync.Server.Handler.Import
  ( module X
  , module Smos.Sync.Server.Handler.Import
  ) where

import GHC.Generics (Generic)

import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.UUID as X

import Text.Show.Pretty as X

import System.Exit

import Control.Concurrent.STM
import Control.Monad.Reader

import Servant

import Path.IO

import Data.Mergeful

import Smos.Sync.API as X

type SyncHandler = ReaderT ServerEnv Handler

data ServerEnv =
  ServerEnv
    { serverEnvStoreVar :: TVar (ServerStore UUID SyncFile)
    }
  deriving (Generic)

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
