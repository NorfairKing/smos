{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Report
  ( smosQueryReport,
  )
where

import Conduit
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Smos.Query.Commands.Entry
import Smos.Query.Commands.Import
import Smos.Report.Report

smosQueryReport :: ReportSettings -> Q ()
smosQueryReport ReportSettings {..} =
  case reportSetReportName of
    Nothing -> liftIO $ T.putStrLn $ availableReportsReport reportSetAvailableReports
    Just reportName ->
      case M.lookup reportName reportSetAvailableReports of
        Nothing -> dieQ $ "No such prepared report configured: " <> T.unpack reportName
        Just PreparedReport {..} ->
          smosQueryEntry
            EntrySettings
              { entrySetFilter = perparedReportFilter,
                entrySetProjection = fromMaybe defaultProjection perparedReportProjection,
                entrySetSorter = preparedReportSorter,
                entrySetHideArchive = fromMaybe HideArchive preparedReportHideArchive,
                entrySetOutputFormat = reportSetOutputFormat
              }

availableReportsReport :: Map Text PreparedReport -> Text
availableReportsReport m =
  if M.null m
    then "No reports configured."
    else
      T.intercalate "\n" $
        "Available reports:" :
        map
          ( \(n, PreparedReport {..}) ->
              T.unwords [n <> ":", fromMaybe "No description" preparedReportDescription]
          )
          (M.toList m)
