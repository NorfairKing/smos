{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Report where

import GHC.Generics (Generic)

import Data.Aeson
import Data.List.NonEmpty (NonEmpty(..))
import Data.Validity

import Smos.Report.Archive
import Smos.Report.Filter
import Smos.Report.Projection
import Smos.Report.Sorter

data PreparedReport =
  PreparedReport
    { perparedReportFilter :: Maybe EntryFilter
    , perparedReportProjection :: Maybe (NonEmpty Projection)
    , preparedReportSorter :: Maybe Sorter
    , preparedReportHideArchive :: Maybe HideArchive
    }
  deriving (Show, Eq, Generic)

instance Validity PreparedReport

instance FromJSON PreparedReport where
  parseJSON =
    withObject "PreparedReport" $ \o ->
      PreparedReport <$> o .:? "filter" <*> o .:? "columns" <*> o .:? "sorter" <*>
      o .:? "hide-archive"
