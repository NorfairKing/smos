{-# LANGUAGE TemplateHaskell #-}

module Smos.Docs.Site.Handler.Nix where

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as TE
import Language.Haskell.TH.Load
import Smos.Docs.Site.Handler.Import
import Smos.Docs.Site.ModuleDocs
import Text.Show.Pretty (ppShow)

getNixosModuleR :: Handler Html
getNixosModuleR = do
  m <- loadIO nixosModuleDocs
  defaultLayout $(widgetFile "nixos-module")
