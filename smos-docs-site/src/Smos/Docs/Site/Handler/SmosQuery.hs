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
import qualified Env
import Options.Applicative
import Options.Applicative.Help
import Smos.Docs.Site.Handler.Import
import Smos.Query.OptParse as Query
import Smos.Report.OptParse.Types as Report
import YamlParse.Applicative as YamlParse

getSmosQueryR :: Handler Html
getSmosQueryR = do
  DocPage {..} <- lookupPage "smos-query"
  let argsHelpText = getHelpPageOf []
      envHelpText = Env.helpDoc Query.prefixedEnvironmentParser
      confHelpText = prettySchemaDoc @Query.Configuration
  defaultLayout $ do
    setSmosTitle "smos-query"
    setDescription "Documentation for the Smos Query Tool"
    $(widgetFile "args")

getSmosQueryCommandR :: Text -> Handler Html
getSmosQueryCommandR cmd = do
  DocPage {..} <- lookupPage $ "smos-query/" <> cmd
  let argsHelpText = getHelpPageOf [T.unpack cmd]
      envHelpText = "This command does not use any extra environment variables." :: String
      confHelpText = case cmd of
        "work" -> confDocsWithKey2 @Report.WorkReportConfiguration @Query.WorkConfiguration workConfigurationKey
        "report" -> confDocsWithKey @Query.PreparedReportConfiguration preparedReportConfigurationKey
        "waiting" -> confDocsWithKey @Query.WaitingConfiguration waitingConfigurationKey
        _ -> "This command admits no extra configuration."
  defaultLayout $ do
    setSmosTitle $ toHtml docPageTitle
    setDescription $ "Documentation for the " <> cmd <> " subcommand of the smos-query tool"
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

confDocsWithKey2 :: forall o1 o2. (YamlSchema o1, YamlSchema o2) => Text -> Text
confDocsWithKey2 key = prettyParserDoc $ objectParser "Configuration" $ optionalFieldWith' key $ (,) <$> (yamlSchema @o1) <*> (yamlSchema @o2)
