{-# LANGUAGE DeriveGeneric #-}

module Smos.Cursor.Logbook
    ( LogbookCursor(..)
    , makeLogbookCursor
    , rebuildLogbookCursor
    ) where

import GHC.Generics (Generic)

import Data.Validity
import Data.Validity.Time ()

import qualified Data.List.NonEmpty as NE

import Data.Time

import Cursor.NonEmpty

import Smos.Data.Types

data LogbookCursor
    = LogbookCursorOpen UTCTime
                        (Maybe (NonEmptyCursor LogbookEntry))
    | LogbookCursorClosed (Maybe (NonEmptyCursor LogbookEntry))
    deriving (Show, Eq, Generic)

instance Validity LogbookCursor

makeLogbookCursor :: Logbook -> LogbookCursor
makeLogbookCursor l =
    case l of
        (LogOpen ut es) ->
            LogbookCursorOpen ut $ makeNonEmptyCursor <$> NE.nonEmpty es
        (LogClosed es) ->
            LogbookCursorClosed $ makeNonEmptyCursor <$> NE.nonEmpty es

rebuildLogbookCursor :: LogbookCursor -> Logbook
rebuildLogbookCursor lc =
    case lc of
        LogbookCursorOpen ut mnec ->
            LogOpen ut $ maybe [] NE.toList $ rebuildNonEmptyCursor <$> mnec
        LogbookCursorClosed mnec ->
            LogClosed $ maybe [] NE.toList $ rebuildNonEmptyCursor <$> mnec
