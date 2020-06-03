{-# LANGUAGE RecordWildCards #-}

module Smos.Web.Server.Serve where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Wai
import Smos.Web.Server.Application ()
import Smos.Web.Server.Constants
import Smos.Web.Server.Foundation
import Smos.Web.Server.OptParse.Types
import Smos.Web.Server.Static
import Text.Show.Pretty
import Yesod

serveSmosWebServer :: ServeSettings -> IO ()
serveSmosWebServer ss = do
  pPrint ss
  runSmosWebServer ss

runSmosWebServer :: ServeSettings -> IO ()
runSmosWebServer ServeSettings {..} = do
  let app = App {appLogLevel = serveSetLogLevel, appStatic = smosWebServerStatic}
  let defMiddles = defaultMiddlewaresNoLogging
  let extraMiddles =
        if development
          then Wai.logStdoutDev
          else Wai.logStdout
  let middle = extraMiddles . defMiddles
  plainApp <- liftIO $ toWaiAppPlain app
  let application = middle plainApp
  Warp.run serveSetPort application
