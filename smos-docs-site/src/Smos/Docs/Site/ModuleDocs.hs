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
  $$( Code $ do
        let embedWith = embedReadTextFileWith moduleDocFunc [||moduleDocFunc||] mode
        md <- runIO $ lookupEnv "SMOS_DOCS_NIXOS_MODULE_DOCS"
        case md of
          Nothing -> do
            runIO $ putStrLn "WARNING: Building without nixos module docs, set SMOS_DOCS_NIXOS_MODULE_DOCS to build them during development."
            examineCode [||BakedIn []||]
          Just mdf -> do
            runIO $ putStrLn $ "Building with nixos module documentation at " <> mdf
            let rf = Path mdf -- Very hacky because it's not necessarily relative
            examineCode $ embedWith rf
    )

homeManagerModuleDocs :: Load [(Text, ModuleOption)]
homeManagerModuleDocs =
  $$( Code $ do
        let embedWith = embedReadTextFileWith homeManagerModuleDocFunc [||homeManagerModuleDocFunc||] mode
        md <- runIO $ lookupEnv "SMOS_DOCS_HOME_MANAGER_MODULE_DOCS"
        case md of
          Nothing -> do
            runIO $ putStrLn "WARNING: Building without home manager module docs, set SMOS_DOCS_HOME_MANAGER_MODULE_DOCS to build them during development."
            examineCode [||BakedIn []||]
          Just mdf -> do
            runIO $ putStrLn $ "Building with home manager module documentation at " <> mdf
            let rf = Path mdf -- Very hacky because it's not necessarily relative
            examineCode $ embedWith rf
    )
