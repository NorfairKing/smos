{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Docs.Site.Handler.Nix where

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as TE
import Language.Haskell.TH.Load
import Smos.Docs.Site.Handler.Import
import Smos.Docs.Site.ModuleDocs

getNixosModuleR :: Handler Html
getNixosModuleR = do
  options <- loadIO nixosModuleDocs
  let title = "Nixos Module Reference"
  let description = "Generated reference documentation about the nixos module for smos server deployments."
  let prettyJSON = TE.decodeUtf8 . LB.toStrict . encodePretty
  defaultLayout $ do
    setTitle title
    setDescriptionIdemp description
    let optionDocs = $(widgetFile "option-docs")
    $(widgetFile "nixos-module")

getHomeManagerModuleR :: Handler Html
getHomeManagerModuleR = do
  options <- loadIO homeManagerModuleDocs
  let title = "Home Manager Module Reference"
  let description = "Generated reference documentation about the home manager module for smos server deployments."
  let prettyJSON = TE.decodeUtf8 . LB.toStrict . encodePretty
  defaultLayout $ do
    setTitle title
    setDescriptionIdemp description
    let optionDocs = $(widgetFile "option-docs")
    $(widgetFile "home-manager-module")
