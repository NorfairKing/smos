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
renderEvents Events {..} = Node (newEntry $ fromMaybe "Invalid title" $ eventsTitle >>= header) $ map (toNode . renderEvent) events
  where
    toNode :: a -> Tree a
    toNode a = Node a []

renderEvent :: Event -> Entry
renderEvent ev@Event {..} =
  let Static {..} = eventStatic
      h = fromMaybe "Event without Summary" $ staticSummary >>= header
      mc = case staticDescription of
        Nothing -> Nothing
        Just desc -> Just $ fromMaybe "invalid description" $ contents desc
      ts = renderTimestamps ev
   in (newEntry h) {entryContents = mc, entryTimestamps = ts}

renderTimestamps :: Event -> Map TimestampName Timestamp
renderTimestamps Event {..} =
  M.fromList $
    concat
      [ [("BEGIN", ts) | Just ts <- [eventStart]],
        [("END", ts) | Just ts <- [eventEnd]]
      ]
