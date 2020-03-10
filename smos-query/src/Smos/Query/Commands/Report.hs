{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Report
  ( smosQueryReport
  ) where

import Conduit

import Data.Map as M
import Data.Maybe
import Data.Text as T

import System.Exit

import Smos.Report.Projection
import Smos.Report.Report

import Smos.Query.Commands.Entry
import Smos.Query.Config
import Smos.Query.OptParse.Types

smosQueryReport :: ReportSettings -> Q ()
smosQueryReport ReportSettings {..} =
  case M.lookup reportSetReportName reportSetAvailableReports of
    Nothing -> liftIO $ die $ "No such prepared report configured: " <> T.unpack reportSetReportName
    Just PreparedReport {..} ->
      smosQueryEntry
        EntrySettings
          { entrySetFilter = perparedReportFilter
          , entrySetProjection = fromMaybe defaultProjection perparedReportProjection
          , entrySetSorter = preparedReportSorter
          , entrySetHideArchive = fromMaybe HideArchive preparedReportHideArchive
          }
