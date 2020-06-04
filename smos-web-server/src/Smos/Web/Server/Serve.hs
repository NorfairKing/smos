{-# LANGUAGE RecordWildCards #-}

module Smos.Web.Server.Serve where

import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Wai
import Servant.Client
import qualified Smos.Server.OptParse.Types as API
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
  burl <- parseBaseUrl $ "http://localhost:" <> show (API.serveSetPort serveSetAPISettings)
  man <- liftIO $ Http.newManager Http.tlsManagerSettings
  loginVar <- liftIO $ newTVarIO M.empty
  let app = App {appLogLevel = serveSetLogLevel, appStatic = smosWebServerStatic, appAPIBaseUrl = burl, appLoginTokens = loginVar, appHttpManager = man}
  let defMiddles = defaultMiddlewaresNoLogging
  let extraMiddles =
        if development
          then Wai.logStdoutDev
          else Wai.logStdout
  let middle = extraMiddles . defMiddles
  plainApp <- liftIO $ toWaiAppPlain app
  let application = middle plainApp
  Warp.run serveSetPort application
