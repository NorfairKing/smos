{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.Changelog
  ( getChangelogR,
  )
where

import Data.SemVer as Version (toString)
import Smos.Client
import Smos.Data
import Smos.Docs.Site.Handler.Import

getChangelogR :: Handler Html
getChangelogR = do
  DocPage {..} <- lookupPage "changelog"
  defaultLayout $ do
    setSmosTitle "Changelog"
    setDescription "The changelog for all of the Smos tools and libraries"
    $(widgetFile "changelog")
