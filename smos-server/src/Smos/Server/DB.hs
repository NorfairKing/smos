{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
import Data.Word
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
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


StripeCustomer
    user UserId
    customer Text -- Stripe customer id

    UniqueStripeCustomer user customer

    deriving Show
    deriving Eq


Subscription
    user UserId
    end UTCTime

    UniqueSubscriptionUser user

    deriving Show
    deriving Eq


ServerFile
    user UserId
    path (Path Rel File)
    contents ByteString
    time ServerTime

    UniqueServerFilePath user path

    deriving Show
    deriving Eq


Backup
    user UserId
    uuid BackupUUID
    time UTCTime
    size Word64

    UniqueBackupUUID user uuid

    deriving Show
    deriving Eq


BackupFile
    backup BackupId
    path (Path Rel File)
    contents Compressed

    deriving Show
    deriving Eq
|]

instance PersistField TZLabel where
  toPersistValue = toPersistValue . renderTZLabel
  fromPersistValue pv = do
    bs <- fromPersistValue pv
    left T.pack $ parseTZLabel bs

instance PersistFieldSql TZLabel where
  sqlType Proxy = sqlType (Proxy :: Proxy Text)
