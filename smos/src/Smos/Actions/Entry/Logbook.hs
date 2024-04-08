{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Entry.Logbook
  ( allLogbookPlainActions,
    logbookClockIn,
    logbookClockOut,
  )
where

import Data.Time
import Smos.Actions.Utils
import Smos.Types

allLogbookPlainActions :: [Action]
allLogbookPlainActions = [logbookClockIn, logbookClockOut]

logbookClockIn :: Action
logbookClockIn =
  Action
    { actionName = "logbookClockIn",
      actionFunc = modifyLogbookCursorSM $ \lbc -> do
        now <- liftIO getCurrentTime
        pure $ logbookCursorClockIn now lbc,
      actionDescription = "Clock in the currently selected entry."
    }

logbookClockOut :: Action
logbookClockOut =
  Action
    { actionName = "logbookClockOut",
      actionFunc = modifyLogbookCursorSM $ \lbc -> do
        now <- liftIO getCurrentTime
        pure $ logbookCursorClockOut now lbc,
      actionDescription = "Clock out the currently selected entry."
    }
