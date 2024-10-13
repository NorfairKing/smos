{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosCalendarImport (getSmosCalendarImportR) where

import Smos.Calendar.Import.OptParse as CalendarImport
import Smos.Docs.Site.Handler.Import

getSmosCalendarImportR :: Handler Html
getSmosCalendarImportR = makeSettingsPage @CalendarImport.Settings "smos-calendar-import"
