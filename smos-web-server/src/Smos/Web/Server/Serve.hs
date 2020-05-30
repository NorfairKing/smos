{-# LANGUAGE RecordWildCards #-}

module Smos.Web.Server.Serve where

import Smos.Web.Server.OptParse.Types
import Text.Show.Pretty

serveSmosWebServer :: ServeSettings -> IO ()
serveSmosWebServer ss@ServeSettings {..} = do
  pPrint ss
