{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Smos.Sync.Client.Command.Login where

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Hashable
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Validity.UUID ()

import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader

import System.Exit
import System.FileLock

import Servant.Client

import Path
import Path.IO

import qualified Data.Mergeful as Mergeful
import qualified Data.Mergeful.Timed as Mergeful

import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP

import Database.Persist.Sqlite as DB

import Smos.Client

import Smos.Sync.Client.Contents
import Smos.Sync.Client.ContentsMap (ContentsMap(..))
import Smos.Sync.Client.DB
import Smos.Sync.Client.Env
import Smos.Sync.Client.OptParse
import Smos.Sync.Client.OptParse.Types
import Smos.Sync.Client.Prompt
import Smos.Sync.Client.Query
import Smos.Sync.Client.Session

loginSmosSyncClient :: Settings -> LoginSettings -> IO ()
loginSmosSyncClient Settings {..} LoginSettings =
  withClientEnv setServerUrl $ \cenv -> withLogin cenv setUsername setPassword (const$ pure ())
