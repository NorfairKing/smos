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

import Path

import Smos.Data

import Smos.Report.Config
import Smos.Report.ShouldPrint
import Smos.Report.Streaming

produceNextActionReport :: SmosReportConfig -> IO [NextActionEntry]
produceNextActionReport src = do
    wd <- agendaFileSpecGetWorkDir (smosReportConfigAgendaFileSpec src)
    sourceToList $
        sourceFilesInNonHiddenDirsRecursively wd .| filterSmosFiles .|
        parseSmosFiles wd .|
        printShouldPrint PrintWarning .|
        smosFileEntries .|
        C.filter (isNextAction . snd) .|
        C.map (uncurry makeNextActionEntry)

isNextAction :: Entry -> Bool
isNextAction entry =
    or $
    (==) (entryState entry) . Just <$> mapMaybe todoState ["NEXT", "STARTED"]

makeNextActionEntry :: Path Rel File -> Entry -> NextActionEntry
makeNextActionEntry rf e =
    NextActionEntry
        { nextActionEntryTodoState = entryState e
        , nextActionEntryHeader = entryHeader e
        , nextActionEntryFilePath = rf
        }

data NextActionEntry = NextActionEntry
    { nextActionEntryTodoState :: Maybe TodoState
    , nextActionEntryHeader :: Header
    , nextActionEntryFilePath :: Path Rel File
    } deriving (Show, Eq, Generic)

instance Validity NextActionEntry
