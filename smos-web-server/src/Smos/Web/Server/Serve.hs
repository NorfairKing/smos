{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Web.Server.Serve where

import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Wai
import Path.IO
import Paths_smos_web_server
import Servant.Client
import Smos.Client
import Smos.Web.Server.Application ()
import Smos.Web.Server.Constants
import Smos.Web.Server.Foundation
import Smos.Web.Server.OptParse.Types
import Smos.Web.Server.Static
import Smos.Web.Style
import Text.Show.Pretty
import Yesod

serveSmosWebServer :: Settings -> IO ()
serveSmosWebServer ss = do
  pPrint ss
  runSmosWebServer ss

runSmosWebServer :: Settings -> IO ()
runSmosWebServer Settings {..} = do
  -- Just to make sure we don't get into trouble with reading files from here.
  -- This also allows to error out early if something is wrong with permissions.
  ensureDir settingDataDir
  let managerSets =
        Http.tlsManagerSettings
          { Http.managerModifyRequest = \request -> do
              let headers =
                    ( "User-Agent",
                      TE.encodeUtf8 $ T.pack $ "smos-web-server-" <> showVersion version
                    ) :
                    -- TODO: Do this via yesod's 'getCurrentRoute' on a case-by-case basis
                    -- so that we have the exact path as well when we get `servant-client >=0.17`.
                    -- We can then also add the username to it.
                    -- http://hackage.haskell.org/package/yesod-core-1.6.19.0/docs/Yesod-Core-Handler.html#v:getCurrentRoute
                    ("Referer", TE.encodeUtf8 $ T.pack $ showBaseUrl settingWebUrl) : Http.requestHeaders request
              pure $ request {Http.requestHeaders = headers}
          }
  man <- liftIO $ Http.newManager managerSets
  loginVar <- liftIO $ newTVarIO M.empty
  let app =
        App
          { appLogLevel = settingLogLevel,
            appStyle = smosWebStyle,
            appStatic = smosWebServerStatic,
            appAPIBaseUrl = settingAPIUrl,
            appDocsBaseUrl = settingDocsUrl,
            appLoginTokens = loginVar,
            appHttpManager = man,
            appDataDir = settingDataDir,
            appGoogleAnalyticsTracking = settingGoogleAnalyticsTracking,
            appGoogleSearchConsoleVerification = settingGoogleSearchConsoleVerification
          }
  withServerVersionCheck app $ do
    let defMiddles = defaultMiddlewaresNoLogging
    let extraMiddles =
          if development
            then Wai.logStdoutDev
            else Wai.logStdout
    let middle = extraMiddles . defMiddles
    plainApp <- liftIO $ toWaiAppPlain app
    let application = middle plainApp
    Warp.run settingPort application

-- | Check whether the smos-server version is supported.
--
-- This also checks whether the smos-server is online.
withServerVersionCheck :: App -> IO a -> IO a
withServerVersionCheck app func = do
  let cenv = mkClientEnv (appHttpManager app) (appAPIBaseUrl app)
  withClientVersionCheck cenv func
