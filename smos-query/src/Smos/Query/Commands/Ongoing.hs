{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Ongoing
  ( smosQueryOngoing,
  )
where

import Conduit
import Smos.Query.Commands.Import
import Smos.Report.Ongoing

smosQueryOngoing :: OngoingSettings -> Q ()
smosQueryOngoing OngoingSettings {..} = do
  dc <- asks envDirectorySettings
  sp <- getShouldPrint
  zone <- liftIO loadLocalTZ
  now <- liftIO getCurrentTime
  report <- liftIO $ produceOngoingReport zone now ongoingSetFilter ongoingSetHideArchive sp dc

  colourSettings <- asks envColourSettings
  outputChunks $ renderOngoingReport zone now colourSettings report

renderOngoingReport :: TZ -> UTCTime -> ColourSettings -> OngoingReport -> [Chunk]
renderOngoingReport zone now colourSettings =
  formatAsBicolourTable colourSettings
    . map (formatOngoingEntry zone now)
    . ongoingReportEntries

formatOngoingEntry :: TZ -> UTCTime -> OngoingEntry -> [Chunk]
formatOngoingEntry zone now OngoingEntry {..} =
  [ pathChunk ongoingEntryFilePath,
    headerChunk ongoingEntryHeader
  ]
    ++ beginEndChunks zone now ongoingEntryBeginEnd

beginEndChunks :: TZ -> UTCTime -> BeginEnd -> [Chunk]
beginEndChunks zone now = \case
  OnlyBegin begin ->
    [ relativeTimestampChunk zone now begin,
      "",
      ""
    ]
  OnlyEnd end ->
    [ "",
      "",
      relativeTimestampChunk zone now end
    ]
  BeginEnd begin end ->
    [ relativeTimestampChunk zone now begin,
      "-",
      relativeTimestampChunk zone now end
    ]
