{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Report where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import Smos.Directory.Archive
import Smos.Report.Filter
import Smos.Report.Projection
import Smos.Report.Sorter

data PreparedReport = PreparedReport
  { preparedReportDescription :: Maybe Text,
    perparedReportFilter :: Maybe EntryFilter,
    perparedReportProjection :: Maybe (NonEmpty Projection),
    preparedReportSorter :: Maybe Sorter,
    preparedReportHideArchive :: Maybe HideArchive
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec PreparedReport)

instance Validity PreparedReport

instance HasCodec PreparedReport where
  codec =
    object "PreparedReport" $
      PreparedReport
        <$> optionalFieldOrNull "description" "A description of the report" .= preparedReportDescription
        <*> optionalFieldOrNull "filter" "The entry filter to get the results in the report" .= perparedReportFilter
        <*> optionalFieldOrNull "columns" "The columns of the report" .= perparedReportProjection
        <*> optionalFieldOrNull "sorter" "The sorter to sort the rows of the report by" .= preparedReportSorter
        <*> optionalFieldOrNull "hide-archive" "Whether to consider the archive for the report" .= preparedReportHideArchive
