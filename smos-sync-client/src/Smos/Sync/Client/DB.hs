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

module Smos.Sync.Client.DB
  ( module Smos.Sync.Client.DB
  , module Database.Persist
  , module Database.Persist.Sql
  ) where

import GHC.Generics (Generic)

import Data.Mergeful.Timed
import Pantry.SHA256

import Path

import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH

import Smos.API

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

ClientFile
    uuid FileUUID sqltype=uuid
    path (Path Rel File)
    hash Int Maybe default=NULL
    sha256 SHA256 Maybe default=NULL
    time ServerTime

    UniquePath path
    UniqueUUID uuid

    deriving Show
    deriving Eq
    deriving Generic
|]
