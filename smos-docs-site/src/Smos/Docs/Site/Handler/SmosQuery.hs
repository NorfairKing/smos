{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosQuery
  ( getSmosQueryR,
    getSmosQueryFilterR,
    getSmosQueryColumnR,
    getSmosQuerySorterR,
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
import Smos.Report.Filter (entryFilterDocs)
import Smos.Report.OptParse.Types as Report
import Smos.Report.Projection (projectionDocs)
import Smos.Report.Sorter (renderSorter, sorterExamples, sorterFormsDocs)

getSmosQueryR :: Handler Html
getSmosQueryR = do
  DocPage {..} <- lookupPage "smos-query"
  let argsHelpText = getHelpPageOf []
      envHelpText = Env.helpDoc Query.prefixedEnvironmentParser
      confHelpText = yamlDesc @Query.Configuration
  defaultLayout $ do
    setSmosTitle "smos-query"
    setDescription "Documentation for the Smos Query Tool"
    $(widgetFile "args")

getSmosQueryFilterR :: Handler Html
getSmosQueryFilterR = do
  DocPage {..} <- lookupPage' ["smos-query", "filter"]
  defaultLayout $ do
    setTitle $ toHtml $ "Smos Query: " <> docPageTitle
    setDescription docPageDescription
    $(widgetFile "smos-query/filter")

getSmosQueryColumnR :: Handler Html
getSmosQueryColumnR = do
  DocPage {..} <- lookupPage' ["smos-query", "column"]
  defaultLayout $ do
    setTitle $ toHtml $ "Smos Query: " <> docPageTitle
    setDescription docPageDescription
    $(widgetFile "smos-query/column")

getSmosQuerySorterR :: Handler Html
getSmosQuerySorterR = do
  DocPage {..} <- lookupPage' ["smos-query", "sorter"]
  defaultLayout $ do
    setTitle $ toHtml $ "Smos Query: " <> docPageTitle
    setDescription docPageDescription
    $(widgetFile "smos-query/sorter")

getSmosQueryCommandR :: Text -> Handler Html
getSmosQueryCommandR cmd = do
  DocPage {..} <- lookupPage' ["smos-query", cmd]
  let argsHelpText = getHelpPageOf [T.unpack cmd]
      envHelpText = "This command does not use any extra environment variables." :: String
      confHelpText = case cmd of
        "work" -> confDocsWithKey @Report.WorkReportConfiguration "work"
        "report" -> confDocsWithKey @Query.PreparedReportConfiguration preparedReportConfigurationKey
        "waiting" -> confDocsWithKey @Report.WaitingReportConfiguration "waiting"
        "stuck" -> confDocsWithKey @Report.StuckReportConfiguration "stuck"
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
