{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosSingle
  ( getSmosSingleR,
  )
where

import Options.Applicative
import Options.Applicative.Help
import Smos.Docs.Site.Handler.Import
import Smos.Single.OptParse
import Smos.Single.OptParse.Types as Single
import YamlParse.Applicative

getSmosSingleR :: Handler Html
getSmosSingleR = do
  DocPage {..} <- lookupPage "smos-single"
  let argsHelpText = getHelpPageOf []
      envHelpText = "TODO" :: String
      confHelpText = prettySchemaDoc @Single.Configuration
  defaultLayout $ do
    setTitle "Smos Documentation - smos-single"
    $(widgetFile "args")

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runArgumentsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "smos-single"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
