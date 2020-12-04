{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Report
  ( smosQueryReport,
  )
where

import Conduit
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Smos.Query.Commands.Entry
import Smos.Query.Config
import Smos.Query.OptParse.Types
import Smos.Report.Projection
import Smos.Report.Report
import System.Exit

smosQueryReport :: ReportSettings -> Q ()
smosQueryReport ReportSettings {..} =
  case reportSetReportName of
    Nothing -> liftIO $ T.putStrLn $ availableReportsReport reportSetAvailableReports
    Just reportName ->
      case M.lookup reportName reportSetAvailableReports of
        Nothing -> liftIO $ die $ "No such prepared report configured: " <> T.unpack reportName
        Just PreparedReport {..} ->
          smosQueryEntry
            EntrySettings
              { entrySetFilter = perparedReportFilter,
                entrySetProjection = fromMaybe defaultProjection perparedReportProjection,
                entrySetSorter = preparedReportSorter,
                entrySetHideArchive = fromMaybe HideArchive preparedReportHideArchive
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
