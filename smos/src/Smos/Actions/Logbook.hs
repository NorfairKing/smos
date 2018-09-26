{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions.Logbook
    ( allLogbookPlainActions
    , allLogbookUsingCharActions
    , logbookClockIn
    ) where

import Data.Time

import Smos.Types

import Smos.Actions.Utils

allLogbookPlainActions :: [Action]
allLogbookPlainActions = [logbookClockIn]

allLogbookUsingCharActions :: [ActionUsing Char]
allLogbookUsingCharActions = []

logbookClockIn :: Action
logbookClockIn =
    Action
    { actionName = "logbookClockIn"
    , actionFunc =
          modifyLogbookCursorSM $ \lbc -> do
              now <- liftIO getCurrentTime
              pure $ logbookCursorClockIn now lbc
    , actionDescription = "Clock in the currently selected entry."
    }
