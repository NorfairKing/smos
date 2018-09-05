module Smos.Cursor.StateHistory
    ( StateHistoryCursor
    , makeStateHistoryCursor
    , rebuildStateHistoryCursor
    , stateHistoryCursorModTodoState
    , stateHistoryCursorSetTodoState
    , stateHistoryCursorToggleTodoState
    , stateHistoryCursorUnsetTodoState
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Time

import Cursor.Simple.List.NonEmpty

import Smos.Data.Types

type StateHistoryCursor = NonEmptyCursor StateHistoryEntry

makeStateHistoryCursor :: NonEmpty StateHistoryEntry -> StateHistoryCursor
makeStateHistoryCursor = makeNonEmptyCursor

rebuildStateHistoryCursor :: StateHistoryCursor -> NonEmpty StateHistoryEntry
rebuildStateHistoryCursor = rebuildNonEmptyCursor

stateHistoryCursorModTodoState ::
       UTCTime
    -> (Maybe TodoState -> Maybe TodoState)
    -> Maybe StateHistoryCursor
    -> StateHistoryCursor
stateHistoryCursorModTodoState now func mshc =
    case mshc of
        Nothing ->
            singletonNonEmptyCursor $ StateHistoryEntry (func Nothing) now
        Just shc ->
            case rebuildNonEmptyCursor shc of
                StateHistoryEntry mts _ :| _ ->
                    nonEmptyCursorAppendAtEnd
                        (StateHistoryEntry (func mts) now)
                        shc

stateHistoryCursorSetTodoState ::
       UTCTime -> TodoState -> Maybe StateHistoryCursor -> StateHistoryCursor
stateHistoryCursorSetTodoState t ts =
    stateHistoryCursorModTodoState t $ const $ Just ts

stateHistoryCursorToggleTodoState ::
       UTCTime -> TodoState -> Maybe StateHistoryCursor -> StateHistoryCursor
stateHistoryCursorToggleTodoState t ts =
    stateHistoryCursorModTodoState t $ \mts ->
        case mts of
            Nothing -> Just ts
            Just ts' ->
                if ts == ts'
                    then Nothing
                    else Just ts

stateHistoryCursorUnsetTodoState ::
       UTCTime -> Maybe StateHistoryCursor -> StateHistoryCursor
stateHistoryCursorUnsetTodoState t =
    stateHistoryCursorModTodoState t $ const Nothing
