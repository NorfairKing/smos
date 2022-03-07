{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosWebServer
  ( getSmosWebServerR,
  )
where

import qualified Env
import Options.Applicative
import Options.Applicative.Help
import Smos.Docs.Site.Handler.Import
import Smos.Web.Server.OptParse as WebServer

getSmosWebServerR :: Handler Html
getSmosWebServerR = do
  DocPage {..} <- lookupPage "smos-web-server"
  let argsHelpText = getHelpPageOf []
      envHelpText = Env.helpDoc WebServer.prefixedEnvironmentParser
      confHelpText = yamlDesc @WebServer.Configuration
  defaultLayout $ do
    setSmosTitle "smos-web-server"
    setDescription "Documentation for the Smos Web Server"
    $(widgetFile "args")

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runFlagsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "smos-web-server"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
