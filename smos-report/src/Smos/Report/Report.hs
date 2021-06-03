{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Report where

import Data.Aeson
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import Smos.Report.Archive
import Smos.Report.Filter
import Smos.Report.Projection
import Smos.Report.Sorter
import YamlParse.Applicative

data PreparedReport = PreparedReport
  { preparedReportDescription :: Maybe Text,
    perparedReportFilter :: Maybe EntryFilter,
    perparedReportProjection :: Maybe (NonEmpty Projection),
    preparedReportSorter :: Maybe Sorter,
    preparedReportHideArchive :: Maybe HideArchive
  }
  deriving (Show, Eq, Generic)

instance Validity PreparedReport

instance ToJSON PreparedReport where
  toJSON PreparedReport {..} =
    object
      [ "description" .= preparedReportDescription,
        "filter" .= perparedReportFilter,
        "columns" .= perparedReportProjection,
        "sorter" .= preparedReportSorter,
        "hide-archive" .= preparedReportHideArchive
      ]

instance FromJSON PreparedReport where
  parseJSON = viaYamlSchema

instance YamlSchema PreparedReport where
  yamlSchema =
    objectParser "PreparedReport" $
      PreparedReport <$> optionalField "description" "A description of the report"
        <*> optionalField "filter" "The entry filter to get the results in the report"
        <*> optionalField "columns" "The columns of the report"
        <*> optionalField "sorter" "The sorter to sort the rows of the report by"
        <*> optionalField "hide-archive" "Whether to consider the archive for the report"
