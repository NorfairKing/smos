{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Render where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Tree
import Smos.Calendar.Import.Event
import Smos.Calendar.Import.Static
import Smos.Data

renderAllEvents :: [Events] -> SmosFile
renderAllEvents = SmosFile . map renderEvents

renderEvents :: Events -> Tree Entry
renderEvents Events {..} =
  Node ((newEntry h) {entryContents = mc}) $
    map (toNode . renderEvent h) events
  where
    Static {..} = eventsStatic
    h = fromMaybe "Event without Summary" $ staticSummary >>= header
    mc = case staticDescription of
      Nothing -> Nothing
      Just "" -> Nothing
      Just desc -> Just $ fromMaybe "invalid description" $ contents desc
    toNode :: a -> Tree a
    toNode a = Node a []

renderEvent :: Header -> Event -> Entry
renderEvent h ev@Event {..} =
  let ts = renderTimestamps ev
   in (newEntry h) {entryTimestamps = ts}

renderTimestamps :: Event -> Map TimestampName Timestamp
renderTimestamps Event {..} =
  M.fromList $
    concat
      [ [("BEGIN", ts) | Just ts <- [eventStart]],
        [("END", ts) | Just ts <- [eventEnd]]
      ]
