{-# LANGUAGE RecordWildCards #-}

module Smos.Web.Server.Serve where

import Smos.Web.Server.Application ()
import Smos.Web.Server.Foundation
import Smos.Web.Server.OptParse.Types
import Text.Show.Pretty
import qualified Yesod

serveSmosWebServer :: ServeSettings -> IO ()
serveSmosWebServer ss@ServeSettings {..} = do
  let app = App {appLogLevel = serveSetLogLevel}
  pPrint ss
  Yesod.warp serveSetPort app
