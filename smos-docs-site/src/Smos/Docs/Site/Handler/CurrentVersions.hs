{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.CurrentVersions
  ( getCurrentVersionsR,
  )
where

import Data.SemVer as Version (toString)
import Smos.Client
import Smos.Data
import Smos.Docs.Site.Handler.Import

getCurrentVersionsR :: Handler Html
getCurrentVersionsR = do
  currentVersions <- lookupPage "current-versions"
  defaultLayout $ do
    setSmosTitle "Current Versions"
    setDescriptionIdemp "The currently supported data format and API versions with instructions on how to integrate your application with smos"
    $(widgetFile "current-versions")
