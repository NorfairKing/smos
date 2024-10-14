{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosSingle (getSmosSingleR) where

import Smos.Docs.Site.Handler.Import
import Smos.Single.OptParse as Single

getSmosSingleR :: Handler Html
getSmosSingleR = makeSettingsPage @Single.Settings "smos-single"
