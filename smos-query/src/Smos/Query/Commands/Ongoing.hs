{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Ongoing
  ( smosQueryOngoing,
  )
where

import Conduit
import qualified Data.Text as T
import Smos.Query.Commands.Import
import Smos.Report.Ongoing
import Text.Printf

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
    [ fore brown $ chunk $ timestampPrettyText begin,
      fore brown $ relativeTimestampChunk zone now begin,
      "",
      "",
      ""
    ]
  OnlyEnd end ->
    [ "",
      "",
      "",
      fore brown $ relativeTimestampChunk zone now end
    ]
  BeginEnd begin end ->
    [ fore brown $ chunk $ timestampPrettyText begin,
      fore brown $ relativeTimestampChunk zone now begin,
      "-",
      fore brown $ chunk $ timestampPrettyText end,
      fore brown $ relativeTimestampChunk zone now end,
      chunk $ T.pack $ beginEndPercentageText (utcToLocalTimeTZ zone now) begin end
    ]

beginEndPercentageText :: LocalTime -> Timestamp -> Timestamp -> String
beginEndPercentageText nowLocal begin end =
  let today = localDay nowLocal
   in case (begin, end) of
        (TimestampDay bd, TimestampDay ed) ->
          printf "% 3d / % 3d" (diffDays today bd + 1) (diffDays ed bd + 1)
        _ ->
          let r :: Float
              r =
                realToFrac (diffLocalTime nowLocal (timestampLocalTime begin))
                  / realToFrac (diffLocalTime (timestampLocalTime end) (timestampLocalTime begin))
           in printf "% 3.f%%" $ 100 * r
