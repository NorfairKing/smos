{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.Changelog
  ( getChangelogR,
  )
where

import Smos.Docs.Site.Changelog
import Smos.Docs.Site.Handler.Import hiding (Header)

getChangelogR :: Handler Html
getChangelogR = do
  DocPage {..} <- lookupPage "changelog"
  defaultLayout $ do
    setTitle "Smos Documentation - Changelog"
    $(widgetFile "page") <> $(widgetFile "changelog")
