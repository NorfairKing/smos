{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosArchive
  ( getSmosArchiveR,
  )
where

import qualified Env
import Options.Applicative
import Options.Applicative.Help
import Smos.Archive.OptParse as Archive
import Smos.Docs.Site.Handler.Import
import YamlParse.Applicative

getSmosArchiveR :: Handler Html
getSmosArchiveR = do
  DocPage {..} <- lookupPage "smos-archive"
  let argsHelpText = getHelpPageOf []
      envHelpText = Env.helpDoc Archive.prefixedEnvironmentParser
      confHelpText = prettySchemaDoc @Archive.Configuration
  defaultLayout $ do
    setSmosTitle "smos-archive"
    setDescription "Documentation for the Smos Archiving tool"
    $(widgetFile "args")

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runArgumentsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "smos-archive"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
