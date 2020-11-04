{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosServer
  ( getSmosServerR,
  )
where

import qualified Env
import Options.Applicative
import Options.Applicative.Help
import Smos.Docs.Site.Handler.Import
import Smos.Server.OptParse as Server
import YamlParse.Applicative

getSmosServerR :: Handler Html
getSmosServerR = do
  DocPage {..} <- lookupPage "smos-server"
  let argsHelpText = getHelpPageOf []
      envHelpText = Env.helpDoc Server.environmentParser
      confHelpText = prettySchemaDoc @Server.Configuration
  defaultLayout $ do
    setSmosTitle "smos-server"
    setDescription "Documentation for the Smos API Server"
    $(widgetFile "args")

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runArgumentsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "smos-server"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
