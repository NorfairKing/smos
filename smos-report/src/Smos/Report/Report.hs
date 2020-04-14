{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Report where

import Data.Aeson
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import Smos.Report.Archive
import Smos.Report.Filter
import Smos.Report.Projection
import Smos.Report.Sorter

data PreparedReport =
  PreparedReport
    { preparedReportDescription :: Maybe Text
    , perparedReportFilter :: Maybe EntryFilter
    , perparedReportProjection :: Maybe (NonEmpty Projection)
    , preparedReportSorter :: Maybe Sorter
    , preparedReportHideArchive :: Maybe HideArchive
    }
  deriving (Show, Eq, Generic)

instance Validity PreparedReport

instance FromJSON PreparedReport where
  parseJSON =
    withObject "PreparedReport" $ \o ->
      PreparedReport <$> o .:? "description" <*> o .:? "filter" <*> o .:? "columns" <*>
      o .:? "sorter" <*>
      o .:? "hide-archive"

instance ToJSON PreparedReport where
  toJSON PreparedReport {..} =
    object
      [ "description" .= preparedReportDescription
      , "filter" .= perparedReportFilter
      , "columns" .= perparedReportProjection
      , "sorter" .= preparedReportSorter
      , "hide-archive" .= preparedReportHideArchive
      ]
