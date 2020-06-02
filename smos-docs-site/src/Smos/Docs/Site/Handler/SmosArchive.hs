{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosArchive
  ( getSmosArchiveR,
  )
where

import Options.Applicative
import Options.Applicative.Help
import Smos.Archive.OptParse
import Smos.Archive.OptParse.Types as Archive
import Smos.Docs.Site.Handler.Import
import YamlParse.Applicative

getSmosArchiveR :: Handler Html
getSmosArchiveR = do
  DocPage {..} <- lookupPage "smos-archive"
  let argsHelpText = getHelpPageOf []
      confHelpText = prettySchemaDoc @Archive.Configuration
  defaultLayout $ do
    setTitle "Smos Documentation - smos-archive"
    $(widgetFile "args")

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runArgumentsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "smos-archive"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
