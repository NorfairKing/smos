{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosSync
  ( getSmosSyncR,
    getSmosSyncNixosR,
    getSmosSyncCommandR,
  )
where

import Data.Text (Text)
import Smos.Docs.Site.Handler.Import
import Smos.Docs.Site.Handler.Page
import Smos.Sync.Client.OptParse as Sync

getSmosSyncR :: Handler Html
getSmosSyncR = makeSettingsPage @Sync.Instructions "smos-sync"

getSmosSyncNixosR :: Handler Html
getSmosSyncNixosR = getPageR ["smos-sync", "nixos"]

getSmosSyncCommandR :: Text -> Handler Html
getSmosSyncCommandR = makeCommandSettingsPage @Sync.Instructions "smos-sync"
