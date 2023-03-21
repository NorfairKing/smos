{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Next where

import Autodocodec
import Conduit
import Control.DeepSeq
import Cursor.Simple.Forest
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics
import Path
import Smos.Data
import Smos.Directory.OptParse.Types
import Smos.Report.Archive
import Smos.Report.Filter
import Smos.Report.ShouldPrint
import Smos.Report.Streaming

produceNextActionReport :: MonadIO m => Maybe EntryFilter -> HideArchive -> ShouldPrint -> DirectorySettings -> m NextActionReport
produceNextActionReport ef ha sp dc = produceReport ha sp dc (nextActionReportConduit ef)

nextActionReportConduit :: Monad m => Maybe EntryFilter -> ConduitT (Path Rel File, SmosFile) void m NextActionReport
nextActionReportConduit ef =
  NextActionReport
    <$> ( nextActionConduitHelper ef
            .| smosCursorCurrents
            .| C.map (uncurry makeNextActionEntry)
            .| sinkList
        )

nextActionConduitHelper :: Monad m => Maybe EntryFilter -> ConduitT (Path Rel File, SmosFile) (Path Rel File, ForestCursor Entry) m ()
nextActionConduitHelper ef =
  smosFileCursors
    .| smosFilter (maybe isNextFilter (FilterAnd isNextFilter) ef)
  where
    isNextFilter :: EntryFilter
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
  deriving stock (Show, Eq, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving (FromJSON, ToJSON) via (Autodocodec NextActionReport)

instance Validity NextActionReport

instance NFData NextActionReport

instance HasCodec NextActionReport where
  codec = dimapCodec NextActionReport nextActionReportEntries codec

data NextActionEntry = NextActionEntry
  { nextActionEntryTodoState :: !(Maybe TodoState),
    nextActionEntryHeader :: !Header,
    nextActionEntryFilePath :: !(Path Rel File) -- The path within the workflow directory
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec NextActionEntry)

instance Validity NextActionEntry

instance NFData NextActionEntry

instance HasCodec NextActionEntry where
  codec =
    object "NextActionEntry" $
      NextActionEntry
        <$> requiredField "state" "The TODO state of the entry" .= nextActionEntryTodoState
        <*> requiredField "header" "The header of the entry" .= nextActionEntryHeader
        <*> requiredField "path" "The path of the file in which this entry was found" .= nextActionEntryFilePath
