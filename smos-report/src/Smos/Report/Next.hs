{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Next where

import Conduit
import Cursor.Simple.Forest
import Data.Aeson
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics
import Path
import Smos.Data
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.Filter
import Smos.Report.ShouldPrint
import Smos.Report.Streaming
import YamlParse.Applicative

produceNextActionReport :: MonadIO m => Maybe EntryFilterRel -> HideArchive -> ShouldPrint -> DirectoryConfig -> m NextActionReport
produceNextActionReport ef ha sp dc = produceReport ha sp dc (nextActionReportConduit ef)

nextActionReportConduit :: Monad m => Maybe EntryFilterRel -> ConduitT (Path Rel File, SmosFile) void m NextActionReport
nextActionReportConduit ef =
  NextActionReport
    <$> ( nextActionConduitHelper ef
            .| smosCursorCurrents
            .| C.map (uncurry makeNextActionEntry)
            .| sinkList
        )

nextActionConduitHelper :: Monad m => Maybe EntryFilterRel -> ConduitT (Path Rel File, SmosFile) (Path Rel File, ForestCursor Entry) m ()
nextActionConduitHelper ef =
  smosFileCursors
    .| smosFilter (maybe isNextFilter (FilterAnd isNextFilter) ef)
  where
    isNextFilter :: EntryFilterRel
    isNextFilter = FilterSnd $ FilterWithinCursor $ FilterEntryTodoState $ FilterMaybe False $ FilterOr (FilterSub "NEXT") (FilterSub "STARTED")

isNextAction :: Entry -> Bool
isNextAction = maybe False isNextTodoState . entryState

isNextTodoState :: TodoState -> Bool
isNextTodoState = (`elem` mapMaybe todoState ["NEXT", "STARTED"])

makeNextActionReport :: [(Path Rel File, Entry)] -> NextActionReport
makeNextActionReport = NextActionReport . map (uncurry makeNextActionEntry)

makeNextActionEntry :: Path Rel File -> Entry -> NextActionEntry
makeNextActionEntry rf e =
  NextActionEntry
    { nextActionEntryTodoState = entryState e,
      nextActionEntryHeader = entryHeader e,
      nextActionEntryFilePath = rf
    }

newtype NextActionReport = NextActionReport
  { nextActionReportEntries :: [NextActionEntry]
  }
  deriving (Show, Eq, Generic, Semigroup, Monoid)

instance Validity NextActionReport

instance FromJSON NextActionReport where
  parseJSON = viaYamlSchema

instance YamlSchema NextActionReport where
  yamlSchema = NextActionReport <$> yamlSchema

instance ToJSON NextActionReport where
  toJSON = toJSON . nextActionReportEntries

data NextActionEntry = NextActionEntry
  { nextActionEntryTodoState :: !(Maybe TodoState),
    nextActionEntryHeader :: !Header,
    nextActionEntryFilePath :: !(Path Rel File) -- The path within the workflow directory
  }
  deriving (Show, Eq, Generic)

instance Validity NextActionEntry

instance FromJSON NextActionEntry where
  parseJSON = viaYamlSchema

instance YamlSchema NextActionEntry where
  yamlSchema =
    objectParser "NextActionEntry" $
      NextActionEntry
        <$> requiredField "state" "The TODO state of the entry"
        <*> requiredField "header" "The header of the entry"
        <*> requiredField "path" "The path of the file in which this entry was found"

instance ToJSON NextActionEntry where
  toJSON NextActionEntry {..} =
    object
      [ "state" .= nextActionEntryTodoState,
        "header" .= nextActionEntryHeader,
        "path" .= nextActionEntryFilePath
      ]
