{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.PreparedReport
  ( smosQueryPreparedReport,
  )
where

import Conduit
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Smos.Query.Commands.Entry
import Smos.Query.Commands.Import
import Smos.Report.Report

smosQueryPreparedReport :: PreparedReportSettings -> Q ()
smosQueryPreparedReport PreparedReportSettings {..} =
  case preparedReportSetReportName of
    Nothing -> liftIO $ T.putStrLn $ availableReportsReport preparedReportSetAvailableReports
    Just reportName ->
      case M.lookup reportName preparedReportSetAvailableReports of
        Nothing -> dieQ $ "No such prepared report configured: " <> T.unpack reportName
        Just PreparedReport {..} ->
          smosQueryEntry
            EntrySettings
              { entrySetFilter = perparedReportFilter,
                entrySetProjection = fromMaybe defaultProjection perparedReportProjection,
                entrySetSorter = preparedReportSorter,
                entrySetHideArchive = fromMaybe HideArchive preparedReportHideArchive,
                entrySetOutputFormat = preparedReportSetOutputFormat
              }

availableReportsReport :: Map Text PreparedReport -> Text
availableReportsReport m =
  if M.null m
    then "No reports configured."
    else
      T.intercalate "\n" $
        "Available reports:"
          : map
            ( \(n, PreparedReport {..}) ->
                T.unwords [n <> ":", fromMaybe "No description" preparedReportDescription]
            )
            (M.toList m)
