{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Render where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tree
import Smos.Calendar.Import.Event
import Smos.Calendar.Import.Static
import Smos.Data

renderAllEvents :: Set Events -> SmosFile
renderAllEvents = makeSmosFile . map renderEvents . S.toAscList

renderEvents :: Events -> Tree Entry
renderEvents Events {..} =
  case eventsList of
    [] -> Node titleEntry []
    [e] -> Node (setProperties eventsStatic (setTimestamps e (setContents mc titleEntry))) []
    es -> Node titleEntry $ map (toNode . setProperties eventsStatic . renderEventEntry h mc) es
  where
    eventsList = S.toAscList events
    titleEntry =
      (newEntry h)
        { entryContents = fromMaybe "Invalid original event" . contents <$> staticOriginalEvent,
          entryProperties = maybe M.empty (M.singleton "UID") $ staticUID >>= propertyValue
        }
    Static {..} = eventsStatic
    h = fromMaybe "Event without Summary" $ staticSummary >>= header
    mc = case staticDescription of
      Nothing -> Nothing
      Just "" -> Nothing
      Just desc -> Just $ fromMaybe "invalid description" $ contents desc
    toNode :: a -> Tree a
    toNode a = Node a []

renderEventEntry :: Header -> Maybe Contents -> Event -> Entry
renderEventEntry h mc ev = setContents mc (setTimestamps ev (newEntry h))

setContents :: Maybe Contents -> Entry -> Entry
setContents mc e = e {entryContents = mc}

setTimestamps :: Event -> Entry -> Entry
setTimestamps ev e =
  let ts = renderTimestamps ev
   in e {entryTimestamps = ts}

renderTimestamps :: Event -> Map TimestampName Timestamp
renderTimestamps Event {..} =
  M.fromList $
    concat
      [ [("BEGIN", ts) | Just ts <- [eventStart]],
        [("END", ts) | Just ts <- [eventEnd]]
      ]

setProperties :: Static -> Entry -> Entry
setProperties ev e =
  let ps = renderProperties ev
   in e {entryProperties = ps}

renderProperties :: Static -> Map PropertyName PropertyValue
renderProperties Static {..} =
  let busyPropertyValue = if staticBusy then "true" else "false"
   in M.fromList $ catMaybes [(,) "busy" <$> propertyValue busyPropertyValue]
