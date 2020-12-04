{-# LANGUAGE DeriveGeneric #-}

module Smos.Cursor.Logbook
  ( LogbookCursor (..),
    makeLogbookCursor,
    rebuildLogbookCursor,
    logbookCursorClockIn,
    logbookCursorClockOut,
  )
where

import Control.DeepSeq
import Cursor.Simple.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import Data.Time
import Data.Validity
import Data.Validity.Time ()
import GHC.Generics (Generic)
import Smos.Data.Types

data LogbookCursor
  = LogbookCursorOpen UTCTime (Maybe (NonEmptyCursor LogbookEntry))
  | LogbookCursorClosed (Maybe (NonEmptyCursor LogbookEntry))
  deriving (Show, Eq, Generic)

instance Validity LogbookCursor where
  validate lbc = decorate "It rebuilds to a valid logbook" $ validate $ rebuildLogbookCursor lbc

instance NFData LogbookCursor

makeLogbookCursor :: Logbook -> LogbookCursor
makeLogbookCursor l =
  case l of
    (LogOpen ut es) -> LogbookCursorOpen ut $ makeNonEmptyCursor <$> NE.nonEmpty es
    (LogClosed es) -> LogbookCursorClosed $ makeNonEmptyCursor <$> NE.nonEmpty es

rebuildLogbookCursor :: LogbookCursor -> Logbook
rebuildLogbookCursor lc =
  case lc of
    LogbookCursorOpen ut mnec -> LogOpen ut $ maybe [] NE.toList $ rebuildNonEmptyCursor <$> mnec
    LogbookCursorClosed mnec -> LogClosed $ maybe [] NE.toList $ rebuildNonEmptyCursor <$> mnec

logbookCursorClockIn :: UTCTime -> LogbookCursor -> Maybe LogbookCursor
logbookCursorClockIn utct lbc =
  case lbc of
    LogbookCursorClosed lbes -> constructValid $ LogbookCursorOpen utct lbes
    LogbookCursorOpen _ _ -> Nothing

logbookCursorClockOut :: UTCTime -> LogbookCursor -> Maybe LogbookCursor
logbookCursorClockOut utct lbc =
  case lbc of
    LogbookCursorOpen u lbes ->
      constructValid $
        LogbookCursorClosed $
          let e = LogbookEntry {logbookEntryStart = u, logbookEntryEnd = utct}
           in Just $
                case lbes of
                  Nothing -> singletonNonEmptyCursor e
                  Just ne -> nonEmptyCursorInsertAtStart e ne
    LogbookCursorClosed _ -> Nothing
