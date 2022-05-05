module Smos.Server.DBSpec (spec) where

import Smos.Server.DB
import Test.Syd
import Test.Syd.Persistent.Sqlite

spec :: Spec
spec = do
  sqliteMigrationSucceedsSpec "test_resources/migration.sql" serverAutoMigration
