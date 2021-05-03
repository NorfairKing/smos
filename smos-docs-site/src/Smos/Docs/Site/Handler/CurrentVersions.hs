{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.CurrentVersions
  ( getCurrentVersionsR,
  )
where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.SemVer as Version (toString)
import Data.Text (Text)
import Data.Time
import Language.Haskell.TH.Load
import Smos.Client
import Smos.Data
import Smos.Docs.Site.Handler.Import

getCurrentVersionsR :: Handler Html
getCurrentVersionsR = do
  currentVersions <- lookupPage "current-versions"
  defaultLayout $ do
    setSmosTitle "Current Versions"
    setDescription "The currently supported data format and API versions with instructions on how to integrate your application with smos"
    $(widgetFile "current-versions")
