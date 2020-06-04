{-# LANGUAGE TemplateHaskell #-}

module Smos.Server.Constants where

import Language.Haskell.TH
import System.Environment

development :: Bool
development =
  $( do
       md <- runIO $ lookupEnv "DEVELOPMENT"
       pure $ ConE $ case md of
         Nothing -> 'False
         Just _ -> 'True
   )
