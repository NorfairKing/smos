{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Entry
  ( smosQueryEntry,
  )
where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Yaml.Builder as Yaml
import Smos.Query.Commands.Import

smosQueryEntry :: EntrySettings -> Q ()
smosQueryEntry EntrySettings {..} = do
  dc <- asks envDirectoryConfig
  sp <- getShouldPrint
  report <- produceEntryReport entrySetFilter entrySetHideArchive sp entrySetProjection entrySetSorter dc
  out <- asks envOutputHandle
  case entrySetOutputFormat of
    OutputPretty -> do
      colourSettings <- asks envColourSettings
      outputChunks $ renderEntryReport colourSettings report
    OutputYaml -> liftIO $ SB.hPutStr out $ Yaml.toByteString report
    OutputJSON -> liftIO $ LB.hPutStr out $ JSON.encode report
    OutputJSONPretty -> liftIO $ LB.hPutStr out $ JSON.encodePretty report
