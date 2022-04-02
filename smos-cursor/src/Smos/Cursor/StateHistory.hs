{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Cursor.StateHistory
  ( StateHistoryCursor (..),
    makeStateHistoryCursor,
    rebuildStateHistoryCursor,
    stateHistoryCursorModTodoState,
    stateHistoryCursorSetTodoState,
    stateHistoryCursorToggleTodoState,
    stateHistoryCursorUnsetTodoState,
  )
where

import Control.DeepSeq
import Cursor.Simple.List.NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Time
import Data.Validity
import GHC.Generics (Generic)
import Smos.Data

newtype StateHistoryCursor = StateHistoryCursor
  { stateHistoryCursorNonEmptyCursor :: NonEmptyCursor StateHistoryEntry
  }
  deriving (Show, Eq, Generic)

instance Validity StateHistoryCursor where
  validate shc =
    mconcat
      [ genericValidate shc,
        decorate "it rebuilds to a valid state history" $
          validate $ rebuildStateHistoryCursor (Just shc)
      ]

instance NFData StateHistoryCursor

makeStateHistoryCursor :: StateHistory -> Maybe StateHistoryCursor
makeStateHistoryCursor sh = do
  ne <- NE.nonEmpty $ unStateHistory sh
  pure $ StateHistoryCursor $ makeNonEmptyCursor ne

rebuildStateHistoryCursor :: Maybe StateHistoryCursor -> StateHistory
rebuildStateHistoryCursor mshc =
  case mshc of
    Nothing -> emptyStateHistory
    Just shc ->
      StateHistory $ NE.toList . rebuildNonEmptyCursor $ stateHistoryCursorNonEmptyCursor shc

stateHistoryCursorModTodoState ::
  UTCTime ->
  (Maybe TodoState -> Maybe TodoState) ->
  Maybe StateHistoryCursor ->
  Maybe StateHistoryCursor -- Nothing if the result wouldn't be valid
stateHistoryCursorModTodoState now func mshc = do
  case mshc of
    Nothing -> do
      let e = mkStateHistoryEntry now (func Nothing)
      constructValid $ StateHistoryCursor {stateHistoryCursorNonEmptyCursor = singletonNonEmptyCursor e}
    Just shc ->
      case rebuildNonEmptyCursor $ stateHistoryCursorNonEmptyCursor shc of
        StateHistoryEntry mts _ :| _ -> do
          let e = mkStateHistoryEntry now (func mts)
          nec <- constructValid $ nonEmptyCursorInsertAtStart e $ stateHistoryCursorNonEmptyCursor shc
          constructValid $ shc {stateHistoryCursorNonEmptyCursor = nec}

stateHistoryCursorSetTodoState ::
  UTCTime -> TodoState -> Maybe StateHistoryCursor -> Maybe StateHistoryCursor
stateHistoryCursorSetTodoState t ts = stateHistoryCursorModTodoState t $ const $ Just ts

stateHistoryCursorToggleTodoState ::
  UTCTime -> TodoState -> Maybe StateHistoryCursor -> Maybe StateHistoryCursor
stateHistoryCursorToggleTodoState t ts =
  stateHistoryCursorModTodoState t $ \case
    Nothing -> Just ts
    Just ts' ->
      if ts == ts'
        then Nothing
        else Just ts

stateHistoryCursorUnsetTodoState :: UTCTime -> Maybe StateHistoryCursor -> Maybe StateHistoryCursor
stateHistoryCursorUnsetTodoState t = stateHistoryCursorModTodoState t $ const Nothing
