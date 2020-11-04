{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosSyncClient
  ( getSmosSyncClientR,
  )
where

import qualified Env
import Options.Applicative
import Options.Applicative.Help
import Smos.Docs.Site.Handler.Import
import Smos.Sync.Client.OptParse as Sync
import YamlParse.Applicative

getSmosSyncClientR :: Handler Html
getSmosSyncClientR = do
  DocPage {..} <- lookupPage "smos-sync-client"
  let argsHelpText = getHelpPageOf []
      envHelpText = Env.helpDoc Sync.prefixedEnvironmentParser
      confHelpText = prettySchemaDoc @Sync.Configuration
  defaultLayout $ do
    setTitle "smos-sync-client"
    setDescription "Documentation for the Smos Synchronisation Client"
    $(widgetFile "args")

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runArgumentsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "smos-sync-client"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
