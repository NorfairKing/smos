module Smos.Sync.Client.DBSpec (spec) where

import Smos.Sync.Client.DB
import Test.Syd
import Test.Syd.Persistent.Sqlite

spec :: Spec
spec = do
  sqliteMigrationSucceedsSpec "test_resources/migration.sql" syncClientAutoMigration
