{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.Changelog
  ( getChangelogR,
    getChangelogAllR,
    getChangelogReleaseR,
  )
where

import Data.List
import Data.Maybe
import Data.Ord
import Data.SemVer as Version (toString)
import Data.Text (Text)
import Smos.Client
import Smos.Data
import Smos.Docs.Site.Handler.Import

getChangelogR :: Handler Html
getChangelogR = do
  changelog <- lookupPage "changelog"
  unreleased <- lookupPage' ["changelog", "unreleased"]
  releases <- lookupPagesIn ["changelog"]
  let mLatestRelease = find ((/= ["unreleased"]) . fst) $ sortOn (Down . fst) releases
  defaultLayout $ do
    setSmosTitle "Changelog"
    setDescription "The changelog for all of the Smos tools and libraries"
    $(widgetFile "changelog")

getChangelogAllR :: Handler Html
getChangelogAllR = undefined

getChangelogReleaseR :: [Text] -> Handler Html
getChangelogReleaseR = undefined
