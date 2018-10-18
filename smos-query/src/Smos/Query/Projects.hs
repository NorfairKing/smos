{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Projects where

import Data.List
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Path
import Text.Printf

import Conduit
import qualified Data.Conduit.Combinators as C
import Rainbow

import Smos.Data

import Smos.Report.Projects
import Smos.Report.Query
import Smos.Report.Streaming

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types

projects :: Q ()
projects = do
    wd <- askWorkDir
    liftIO $ do
        now <- getZonedTime
        tups <-
            sourceToList $
            sourceFilesInNonHiddenDirsRecursively
                (wd </> $(mkRelDir "projects")) .|
            filterSmosFiles .|
            parseSmosFiles .|
            printShouldPrint PrintWarning
        putTableLn $ renderProjectsReport $ map (uncurry makeProjectEntry) tups

renderProjectsReport :: [ProjectEntry] -> Table
renderProjectsReport = formatAsTable . map renderProjectEntry

renderProjectEntry :: ProjectEntry -> [Chunk Text]
renderProjectEntry ProjectEntry {..} =
    rootedPathChunk projectEntryFilePath :
    case projectEntryCurrentEntry of
        Nothing -> [chunk "No next action"]
        Just e@Entry {..} ->
            [mTodoStateChunk $ entryState e, headerChunk entryHeader]
