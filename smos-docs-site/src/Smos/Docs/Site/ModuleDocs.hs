{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Smos.Docs.Site.ModuleDocs
  ( module Smos.Docs.Site.ModuleDocs,
    module Smos.Docs.Site.ModuleDocs.TH,
  )
where

import Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Language.Haskell.TH
import Language.Haskell.TH.Load
import Language.Haskell.TH.Syntax
import Path
import Path.IO
import Path.Internal
import Smos.Docs.Site.Constants
import Smos.Docs.Site.ModuleDocs.TH
import System.Environment
import System.FilePath

nixosModuleDocs :: Load [(Text, ModuleOption)]
nixosModuleDocs =
  $$( do
        let defaultFile = [relfile|static/module-docs.json|]
        let embedWith = embedReadTextFileWith moduleDocFunc [||moduleDocFunc||] mode
        let embedDefault = embedWith defaultFile
        if development
          then do
            md <- runIO $ lookupEnv "MODULE_DOCS"
            case md of
              Nothing -> do
                runIO $ putStrLn "WARNING: Building without nixos module docs"
                [||BakedIn []||]
              Just mdf ->
                let rf = Path mdf -- Very hacky because it's not necessarily relative
                 in embedWith rf
          else embedDefault
    )
