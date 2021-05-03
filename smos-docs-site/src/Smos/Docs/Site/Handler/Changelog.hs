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
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.SemVer as Version (toString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Language.Haskell.TH.Load
import Smos.Client
import Smos.Data
import Smos.Docs.Site.Handler.Import

showReleaseDay :: Day -> Text
showReleaseDay = T.pack . formatTime defaultTimeLocale "%F"

getChangelogR :: Handler Html
getChangelogR = do
  mUnreleased <- loadIO unreleasedChangelog
  releases <- loadIO changelogs
  let (latestReleaseDay, latestRelease) = M.findMin releases
  defaultLayout $ do
    setSmosTitle "Changelog"
    setDescription "The changelog for the latest release of all of the Smos tools and libraries"
    $(widgetFile "changelog")

getChangelogAllR :: Handler Html
getChangelogAllR = do
  mUnreleased <- loadIO unreleasedChangelog
  releases <- loadIO changelogs
  defaultLayout $ do
    setSmosTitle "Changelog"
    setDescription "The changelog for all releases of all of the Smos tools and libraries"
    $(widgetFile "changelog-all")

getChangelogReleaseR :: Day -> Handler Html
getChangelogReleaseR day = do
  releases <- loadIO changelogs
  case M.lookup day releases of
    Nothing -> notFound
    Just release -> do
      defaultLayout $ do
        setSmosTitle $ "Changelog for the " <> toHtml (showReleaseDay day) <> " release"
        setDescription $ "The changelog for the " <> showReleaseDay day <> " release of all of the Smos tools and libraries"
        $(widgetFile "changelog-release")
