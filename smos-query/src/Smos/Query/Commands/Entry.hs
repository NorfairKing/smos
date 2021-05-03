{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Entry
  ( smosQueryEntry,
  )
where

import Smos.Query.Commands.Import

smosQueryEntry :: EntrySettings -> Q ()
smosQueryEntry EntrySettings {..} = do
  dc <- asks envDirectoryConfig
  sp <- getShouldPrint
  report <- produceEntryReport entrySetFilter entrySetHideArchive sp entrySetProjection entrySetSorter dc

  colourSettings <- asks envColourSettings
  outputChunks $ renderEntryReport colourSettings report
