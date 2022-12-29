{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.ResolveLocal where

import Control.Monad.Reader
import Data.Functor.Identity
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time
import GHC.Generics (Generic)
import qualified ICal.Component.TimeZone as ICal
import qualified ICal.Conformance as ICal
import qualified ICal.Parameter as ICal
import qualified ICal.Recurrence as ICal
import Smos.Calendar.Import.Event
import Smos.Calendar.Import.UTCEvent
import Smos.Data

resolveUTCEvents :: Set UTCEvents -> IO (Set Events)
resolveUTCEvents = resolveUTCEventsHelper getTimeZone

-- This function only exists so we can run our tests independently of the current timezone in which the tests are run.
resolveUTCEventsInUTC :: Set UTCEvents -> Set Events
resolveUTCEventsInUTC = runIdentity . resolveUTCEventsHelper (\_ -> pure utc)

resolveUTCEventsHelper :: Monad m => (UTCTime -> m TimeZone) -> Set UTCEvents -> m (Set Events)
resolveUTCEventsHelper resolver =
  fmap S.fromList
    . mapM
      ( \UTCEvents {..} -> do
          let eventsStatic = utcEventsStatic
          events <- S.fromList <$> mapM (resolveUTCEvent resolver) (S.toList utcEvents)
          pure Events {..}
      )
    . S.toList

resolveUTCEvent :: Monad m => (UTCTime -> m TimeZone) -> UTCEvent -> m Event
resolveUTCEvent resolver UTCEvent {..} = do
  eventStart <- mapM (resolveTimestampInUTC resolver) utcEventStart
  eventEnd <- mapM (resolveTimestampInUTC resolver) utcEventEnd
  pure Event {..}

resolveTimestampInUTC :: Monad m => (UTCTime -> m TimeZone) -> ICal.Timestamp -> m Timestamp
resolveTimestampInUTC resolver = \case
  ICal.TimestampDay d -> pure $ TimestampDay d
  ICal.TimestampLocalTime lt -> pure $ TimestampLocalTime lt
  ICal.TimestampUTCTime ut -> do
    tz <- resolver ut
    pure $ TimestampLocalTime $ utcToLocalTime tz ut
