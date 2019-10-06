{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Sync.Server.DB
  ( module Smos.Sync.Server.DB
  , module Database.Persist
  , module Database.Persist.Sql
  ) where

import GHC.Generics (Generic)

import Data.ByteString (ByteString)
import Data.Mergeful.Timed

import Path

import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH

import Smos.Sync.API

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|


ServerFile
    uuid FileUUID sqltype=uuid
    path (Path Rel File)
    contents ByteString
    time ServerTime

    UniquePath path
    UniqueUUID uuid

    deriving Show
    deriving Eq
    deriving Generic
|]
