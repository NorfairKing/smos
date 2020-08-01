{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Render where

import qualified Data.Text as T
import Debug.Trace
import Smos.Calendar.Import.Event
import Smos.Data

renderEvent :: Event -> Maybe Entry
renderEvent Event {..} = do
  h <- header eventTitle
  traceShowM h
  let mc = if T.null eventDescription then Nothing else contents eventDescription
  traceShowM mc
  let e = (newEntry h) {entryContents = mc}
  pure e
