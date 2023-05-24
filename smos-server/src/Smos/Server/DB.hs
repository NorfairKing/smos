{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-name-shadowing #-}

module Smos.Server.DB
  ( module Smos.Server.DB,
    module Smos.Server.DB.Compressed,
    module Database.Persist,
    module Database.Persist.Sql,
  )
where

import Control.Arrow (left)
import Data.ByteString (ByteString)
import Data.Mergeful.Timed
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Zones.All
import Data.Validity
import Data.Validity.Persist ()
import Data.Word
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)
import Path
import Smos.API
import Smos.Data
import Smos.Server.DB.Compressed

share
  [mkPersist sqlSettings, mkMigrate "serverAutoMigration"]
  [persistLowerCase|

User
    name Username
    hashedPassword (PasswordHash Bcrypt)
    created UTCTime
    lastLogin UTCTime Maybe default=NULL
    lastUse UTCTime Maybe default=NULL

    UniqueUsername name

    deriving Show
    deriving Eq
    deriving Generic


StripeCustomer
    user UserId
    customer Text -- Stripe customer id

    UniqueStripeCustomer user customer

    deriving Show
    deriving Eq
    deriving Generic


Subscription
    user UserId
    end UTCTime

    UniqueSubscriptionUser user

    deriving Show
    deriving Eq
    deriving Generic


ServerFile
    user UserId
    path (Path Rel File)
    contents ByteString
    time ServerTime

    UniqueServerFilePath user path

    deriving Show
    deriving Eq
    deriving Generic


Backup
    user UserId
    uuid BackupUUID
    time UTCTime
    size Word64

    UniqueBackupUUID user uuid

    deriving Show
    deriving Eq
    deriving Generic


BackupFile
    backup BackupId
    path (Path Rel File)
    contents Compressed

    deriving Show
    deriving Eq
    deriving Generic
|]

instance Validity Backup

instance PersistField TZLabel where
  toPersistValue = toPersistValue . renderTZLabel
  fromPersistValue pv = do
    bs <- fromPersistValue pv
    left T.pack $ parseTZLabel bs

instance PersistFieldSql TZLabel where
  sqlType Proxy = sqlType (Proxy :: Proxy Text)
