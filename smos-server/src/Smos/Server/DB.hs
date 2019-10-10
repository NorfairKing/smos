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

module Smos.Server.DB
  ( module Smos.Server.DB
  , module Database.Persist
  , module Database.Persist.Sql
  ) where

import GHC.Generics (Generic)

import Data.ByteString (ByteString)
import Data.Mergeful.Timed
import Data.Time

import Path

import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH

import Smos.API

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

User
    username Username
    hashedPassword HashedPassword
    created UTCTime

    UniqueUsername username

    deriving Show
    deriving Eq
    deriving Generic


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
