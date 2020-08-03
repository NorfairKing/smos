{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Render where

import Data.Maybe
import qualified Data.Text as T
import Data.Tree
import Smos.Calendar.Import.Event
import Smos.Data

renderEvents :: [Event] -> SmosFile
renderEvents es = SmosFile $ map (\e -> Node e []) $ mapMaybe renderEvent es

renderEvent :: Event -> Maybe Entry
renderEvent Event {..} = do
  let h = fromMaybe "Event without Summary" $ eventSummary >>= header
  mc <- mapM contents eventDescription
  let e = (newEntry h) {entryContents = mc}
  pure e
