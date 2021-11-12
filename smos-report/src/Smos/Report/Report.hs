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
import Smos.Report.Archive
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
        <$> optionalField "description" "A description of the report" .= preparedReportDescription
        <*> optionalField "filter" "The entry filter to get the results in the report" .= perparedReportFilter
        <*> optionalField "columns" "The columns of the report" .= perparedReportProjection
        <*> optionalField "sorter" "The sorter to sort the rows of the report by" .= preparedReportSorter
        <*> optionalField "hide-archive" "Whether to consider the archive for the report" .= preparedReportHideArchive
