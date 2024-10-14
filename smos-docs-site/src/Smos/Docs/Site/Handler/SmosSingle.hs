{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosSingle
  ( getSmosSingleR,
  )
where

import OptEnvConf
import Smos.Docs.Site.Handler.Import
import Smos.Single.OptParse as Single
import Text.Colour

getSmosSingleR :: Handler Html
getSmosSingleR = do
  docPage <- lookupPage "smos-single"
  defaultLayout $ do
    makeSettingsPage @Single.Settings "smos-single" docPage

makeSettingsPage :: forall a. (OptEnvConf.HasParser a) => String -> DocPage -> Widget
makeSettingsPage progname DocPage {..} = do
  let docsChunks = renderReferenceDocumentation progname (parserDocs (settingsParser :: OptEnvConf.Parser a))
  let referenceDocs = renderChunksText WithoutColours docsChunks
  setSmosTitle $ toHtml docPageTitle
  setDescriptionIdemp docPageDescription
  $(widgetFile "settings")
