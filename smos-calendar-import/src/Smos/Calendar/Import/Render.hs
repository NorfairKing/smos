{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Render where

import Smos.Calendar.Import.Event
import Smos.Data

renderEvent :: Event -> Maybe Entry
renderEvent Event {..} = do
  h <- header eventTitle
  mc <- contents <$> eventDescription
  let e = (newEntry h) {entryContents = mc}
  pure e
