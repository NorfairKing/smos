{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Docs.Site.Handler.Nix where

import Language.Haskell.TH.Load
import Smos.Docs.Site.Handler.Import
import Smos.Docs.Site.ModuleDocs

getNixosModuleR :: Handler Html
getNixosModuleR = do
  options <- loadIO nixosModuleDocs
  let title = "Nixos Module Reference"
  let description = "Generated reference documentation about the nixos module for smos server deployments."
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
  defaultLayout $ do
    setTitle title
    setDescriptionIdemp description
    let optionDocs = $(widgetFile "option-docs")
    $(widgetFile "home-manager-module")
