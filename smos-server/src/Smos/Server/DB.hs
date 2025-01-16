{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
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
import System.Exit
import UnliftIO

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
    user UserId OnDeleteCascade
    customer Text -- Stripe customer id

    UniqueStripeCustomer user customer

    deriving Show
    deriving Eq


Subscription
    user UserId OnDeleteCascade
    end UTCTime

    UniqueSubscriptionUser user

    deriving Show
    deriving Eq


ServerFile
    user UserId OnDeleteCascade
    path (Path Rel File)
    contents ByteString
    time ServerTime

    UniqueServerFilePath user path

    deriving Show
    deriving Eq


Backup
    user UserId OnDeleteCascade
    uuid BackupUUID
    time UTCTime
    size Word64

    UniqueBackupUUID user uuid

    deriving Show
    deriving Eq


BackupFile
    backup BackupId OnDeleteCascade
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

completeServerMigration :: (MonadUnliftIO m, MonadLogger m) => Bool -> SqlPersistT m ()
completeServerMigration quiet = do
  logInfoN "Running automatic migrations"
  (if quiet then void . runMigrationQuiet else runMigration) serverAutoMigration
    `catch` ( \case
                PersistError t -> liftIO $ die $ T.unpack t
                e -> throwIO e
            )
  logInfoN "Autmatic migrations done, starting application-specific migrations."
  setUpIndices
  logInfoN "Migrations done."

-- Guidelines for indices:
--
--     * UNIQUE INDEX for uniqueness constraint
--     * INDEX for foreign key
setUpIndices :: (MonadIO m) => SqlPersistT m ()
setUpIndices = do
  rawExecute "CREATE UNIQUE INDEX IF NOT EXISTS user_name ON user (name)" []
  rawExecute "CREATE UNIQUE INDEX IF NOT EXISTS stripe_customer_user_customer ON stripe_customer (user, customer)" []
  rawExecute "CREATE UNIQUE INDEX IF NOT EXISTS subscription_user ON subscription (user)" []
  rawExecute "CREATE UNIQUE INDEX IF NOT EXISTS server_file_user_path ON server_file (user, path)" []
  rawExecute "CREATE INDEX IF NOT EXISTS server_file_path ON server_file (path)" []
  rawExecute "CREATE INDEX IF NOT EXISTS server_file_user ON server_file (user)" []
  rawExecute "CREATE UNIQUE INDEX IF NOT EXISTS backup_user_uuid ON backup (user, uuid)" []
  rawExecute "CREATE INDEX IF NOT EXISTS backup_user ON backup (user)" []
  rawExecute "CREATE INDEX IF NOT EXISTS backup_uuid ON backup (uuid)" []
  rawExecute "CREATE INDEX IF NOT EXISTS backup_file_backup ON backup_file (backup)" []
  rawExecute "CREATE INDEX IF NOT EXISTS backup_file_path ON backup_file (path)" []
