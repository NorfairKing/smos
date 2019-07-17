{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Next where

import GHC.Generics

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.Maybe
import Data.Validity
import Data.Validity.Path ()

import Smos.Data

import Smos.Report.Config
import Smos.Report.Path
import Smos.Report.ShouldPrint
import Smos.Report.Streaming

produceNextActionReport :: SmosReportConfig -> IO [NextActionEntry]
produceNextActionReport src = do
  wd <- resolveWorkflowDir $ smosReportConfigAgendaFileSpec src
  sourceToList $
    sourceFilesInNonHiddenDirsRecursively wd .| filterSmosFiles .| parseSmosFiles .|
    printShouldPrint PrintWarning .|
    smosFileEntries .|
    C.filter (isNextAction . snd) .|
    C.map (uncurry makeNextActionEntry)

isNextAction :: Entry -> Bool
isNextAction = maybe False isNextTodoState . entryState

isNextTodoState :: TodoState -> Bool
isNextTodoState = (`elem` (mapMaybe todoState ["NEXT", "STARTED"]))

makeNextActionEntry :: RootedPath -> Entry -> NextActionEntry
makeNextActionEntry rf e =
  NextActionEntry
    { nextActionEntryTodoState = entryState e
    , nextActionEntryHeader = entryHeader e
    , nextActionEntryFilePath = rf
    }

data NextActionEntry =
  NextActionEntry
    { nextActionEntryTodoState :: Maybe TodoState
    , nextActionEntryHeader :: Header
    , nextActionEntryFilePath :: RootedPath
    }
  deriving (Show, Eq, Generic)

instance Validity NextActionEntry
