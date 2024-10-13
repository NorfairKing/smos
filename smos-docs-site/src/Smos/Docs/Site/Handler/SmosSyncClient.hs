{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosSyncClient
  ( getSmosSyncClientR,
    getSmosSyncClientNixosR,
    getSmosSyncClientDatabaseMigrationR,
    getSmosSyncClientCommandR,
  )
where

import Data.Text (Text)
import Smos.Docs.Site.Handler.Import
import Smos.Docs.Site.Handler.Page
import Smos.Sync.Client.OptParse as SyncClient

getSmosSyncClientR :: Handler Html
getSmosSyncClientR = makeSettingsPage @SyncClient.Instructions "smos-sync-client"

getSmosSyncClientNixosR :: Handler Html
getSmosSyncClientNixosR = getPageR ["smos-sync-client", "nixos"]

getSmosSyncClientDatabaseMigrationR :: Handler Html
getSmosSyncClientDatabaseMigrationR = getPageR ["smos-sync-client", "database-migration"]

getSmosSyncClientCommandR :: Text -> Handler Html
getSmosSyncClientCommandR = makeCommandSettingsPage @SyncClient.Instructions "smos-sync-client"
