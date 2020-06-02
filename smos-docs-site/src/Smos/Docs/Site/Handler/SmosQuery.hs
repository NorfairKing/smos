{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosQuery
  ( getSmosQueryR,
    getSmosQueryCommandR,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import Options.Applicative.Help
import Smos.Docs.Site.Handler.Import
import Smos.Query.OptParse
import Smos.Query.OptParse.Types as Query
import YamlParse.Applicative as YamlParse

getSmosQueryR :: Handler Html
getSmosQueryR = do
  DocPage {..} <- lookupPage "smos-query"
  let argsHelpText = getHelpPageOf []
      confHelpText = prettySchemaDoc @Query.Configuration
  defaultLayout $ do
    setTitle "Smos Documentation - smos-query"
    $(widgetFile "args")

getSmosQueryCommandR :: Text -> Handler Html
getSmosQueryCommandR cmd = do
  DocPage {..} <- lookupPage $ "smos-query_" <> cmd
  let argsHelpText = getHelpPageOf [T.unpack cmd]
      confHelpText = case cmd of
        "work" -> confDocsWithKey @Query.WorkConfiguration workConfigurationKey
        "report" -> confDocsWithKey @Query.PreparedReportConfiguration preparedReportConfigurationKey
        "waiting" -> confDocsWithKey @Query.WaitingConfiguration waitingConfigurationKey
        _ -> "This command admits no extra configuration."
  defaultLayout $ do
    setTitle $ toHtml $ "Smos Documentation - " <> docPageTitle
    $(widgetFile "args")

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runArgumentsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "smos-query"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."

confDocsWithKey :: forall o. YamlSchema o => Text -> Text
confDocsWithKey key = prettyParserDoc $ objectParser "Configuration" $ optionalFieldWith' key (yamlSchema @o)
