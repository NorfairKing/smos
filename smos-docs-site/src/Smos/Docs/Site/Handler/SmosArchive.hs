{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosArchive
  ( getSmosArchiveR,
    getSmosArchiveCommandR,
  )
where

import Data.Text (Text)
import Smos.Archive.OptParse as Archive
import Smos.Docs.Site.Handler.Import

getSmosArchiveR :: Handler Html
getSmosArchiveR = makeSettingsPage @Archive.Instructions "smos-archive"

getSmosArchiveCommandR :: Text -> Handler Html
getSmosArchiveCommandR = makeCommandSettingsPage @Archive.Instructions "smos-archive"
