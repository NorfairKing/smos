{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Projects
  ( smosQueryProjects,
  )
where

import Conduit
import Smos.Query.Commands.Import
import Smos.Report.Projects

smosQueryProjects :: ProjectsSettings -> Q ()
smosQueryProjects ProjectsSettings {..} = do
  projs <-
    sourceToList $
      streamSmosProjectsQ
        .| smosMFilter (FilterFst <$> projectsSetFilter)
  let projectsReport = makeProjectsReport projs
  colourSettings <- asks envColourSettings
  outputChunks $ renderProjectsReport colourSettings projectsReport

renderProjectsReport :: ColourSettings -> ProjectsReport -> [Chunk]
renderProjectsReport colourSettings = formatAsBicolourTable colourSettings . map renderProjectEntry . projectsReportEntries

renderProjectEntry :: ProjectEntry -> [Chunk]
renderProjectEntry ProjectEntry {..} =
  pathChunk projectEntryFilePath :
  case projectEntryCurrentEntry of
    Nothing -> [chunk "No next action"]
    Just e@Entry {..} -> [mTodoStateChunk $ entryState e, headerChunk entryHeader]
