{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosNotify (getSmosNotifyR) where

import Smos.Docs.Site.Handler.Import
import Smos.Notify.OptParse as Notify

getSmosNotifyR :: Handler Html
getSmosNotifyR = makeSettingsPage @Notify.Settings "smos-notify"
