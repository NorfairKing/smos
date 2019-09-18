{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Projects where

import Data.List
import Data.Text (Text)

import Conduit
import Rainbow

import Smos.Data

import Smos.Report.Projects
import Smos.Report.Streaming

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming

projects :: Q ()
projects = do
  tups <-
    sourceToList $
    streamSmosProjects .| parseSmosFiles .| printShouldPrint PrintWarning
  liftIO $
    putTableLn $
    renderProjectsReport $
    sortOn (fmap entryState . projectEntryCurrentEntry) $
    map (uncurry makeProjectEntry) tups

renderProjectsReport :: [ProjectEntry] -> Table
renderProjectsReport = formatAsTable . map renderProjectEntry

renderProjectEntry :: ProjectEntry -> [Chunk Text]
renderProjectEntry ProjectEntry {..} =
  rootedPathChunk projectEntryFilePath :
  case projectEntryCurrentEntry of
    Nothing -> [chunk "No next action"]
    Just e@Entry {..} ->
      [mTodoStateChunk $ entryState e, headerChunk entryHeader]
