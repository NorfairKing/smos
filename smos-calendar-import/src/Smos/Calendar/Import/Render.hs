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
  Node
    ( (newEntry h)
        { entryContents = mc,
          entryProperties = maybe M.empty (M.singleton "UID") $ staticUID >>= propertyValue
        }
    )
    $ map (toNode . renderEvent h mc) (S.toAscList events)
  where
    Static {..} = eventsStatic
    h = fromMaybe "Event without Summary" $ staticSummary >>= header
    mc = case staticDescription of
      Nothing -> Nothing
      Just "" -> Nothing
      Just desc -> Just $ fromMaybe "invalid description" $ contents desc
    toNode :: a -> Tree a
    toNode a = Node a []

renderEvent :: Header -> Maybe Contents -> Event -> Entry
renderEvent h mc ev =
  let ts = renderTimestamps ev
   in (newEntry h) {entryTimestamps = ts, entryContents = mc}

renderTimestamps :: Event -> Map TimestampName Timestamp
renderTimestamps Event {..} =
  M.fromList $
    concat
      [ [("BEGIN", ts) | Just ts <- [eventStart]],
        [("END", ts) | Just ts <- [eventEnd]]
      ]
