{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosSingle (getSmosSingleR) where

import OptEnvConf
import Smos.Docs.Site.Handler.Import
import Smos.Single.OptParse as Single
import Text.Colour

getSmosSingleR :: Handler Html
getSmosSingleR = makeSettingsPage @Single.Settings "smos-single"
