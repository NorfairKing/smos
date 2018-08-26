{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Actions.Entry
    ( entrySelectWhole
    , entrySelectHeader
    , entrySelectContents
    , entrySelectProperties
    , entrySelectTimestamps
    , entrySelectStateHistory
    , entrySelectTags
    , entrySelectLogbook
    ) where

import Smos.Types

import Smos.Actions.Utils

entrySelectWhole :: Action
entrySelectWhole =
    Action
        { actionName = "entrySelectWhole"
        , actionFunc = modifyEntryCursor entryCursorSelectWhole
        , actionDescription = "Select the whole current Entry"
        }

entrySelectHeader :: Action
entrySelectHeader =
    Action
        { actionName = "entrySelectHeader"
        , actionFunc = modifyEntryCursor entryCursorSelectHeader
        , actionDescription = "Select the current Entry's header"
        }

entrySelectContents :: Action
entrySelectContents =
    Action
        { actionName = "entrySelectContents"
        , actionFunc = modifyEntryCursor entryCursorSelectContents
        , actionDescription = "Select the current Entry's contents"
        }

entrySelectTimestamps :: Action
entrySelectTimestamps =
    Action
        { actionName = "entrySelectTimestamps"
        , actionFunc = modifyEntryCursor entryCursorSelectTimestamps
        , actionDescription = "Select the current Entry's timestamps"
        }

entrySelectProperties :: Action
entrySelectProperties =
    Action
        { actionName = "entrySelectProperties"
        , actionFunc = modifyEntryCursor entryCursorSelectProperties
        , actionDescription = "Select the current Entry's properties"
        }

entrySelectStateHistory :: Action
entrySelectStateHistory =
    Action
        { actionName = "entrySelectStateHistory"
        , actionFunc = modifyEntryCursor entryCursorSelectStateHistory
        , actionDescription = "Select the current Entry's state history"
        }

entrySelectTags :: Action
entrySelectTags =
    Action
        { actionName = "entrySelectTags"
        , actionFunc = modifyEntryCursor entryCursorSelectTags
        , actionDescription = "Select the current Entry's tags"
        }

entrySelectLogbook :: Action
entrySelectLogbook =
    Action
        { actionName = "entrySelectLogbook"
        , actionFunc = modifyEntryCursor entryCursorSelectLogbook
        , actionDescription = "Select the current Entry's logbook"
        }
