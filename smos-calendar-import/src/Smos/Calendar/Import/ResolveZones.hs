{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.ResolveZones where

import Control.Monad.Reader
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import qualified ICal.Component.TimeZone as ICal
import qualified ICal.Conformance as ICal
import qualified ICal.Parameter as ICal
import qualified ICal.Recurrence as ICal
import Smos.Calendar.Import.UTCEvent
import Smos.Calendar.Import.UnresolvedEvent

resolveEvents :: Set UnresolvedEvents -> Set UTCEvents
resolveEvents = S.unions . map resolveUnresolvedEvents . S.toList

resolveUnresolvedEvents :: UnresolvedEvents -> Set UTCEvents
resolveUnresolvedEvents UnresolvedEvents {..} =
  let ctx =
        RecurCtx
          { resolveCtxTimeZones = unresolvedEventsTimeZones
          }
   in S.fromList $
        flip mapMaybe (S.toList unresolvedEventGroups) $ \eg ->
          let errOrEvents = ICal.runConform $ runReaderT (resolveUnresolvedEventGroup eg) ctx
           in case errOrEvents of
                Left _ -> Nothing
                Right (events, _) -> Just events

data RecurCtx = RecurCtx
  { resolveCtxTimeZones :: Map ICal.TZIDParam ICal.TimeZone
  }
  deriving (Show, Eq, Generic)

type R a = ReaderT RecurCtx ICal.Resolv a

resolveUnresolvedEventGroup :: UnresolvedEventGroup -> R UTCEvents
resolveUnresolvedEventGroup UnresolvedEventGroup {..} = do
  let utcEventsStatic = unresolvedEventGroupStatic
  utcEvents <- S.fromList <$> mapM resolveEventOccurrence (S.toList unresolvedEvents)
  pure UTCEvents {..}

resolveEventOccurrence :: ICal.EventOccurrence -> R UTCEvent
resolveEventOccurrence eo = do
  zones <- asks resolveCtxTimeZones
  ICal.ResolvedEvent {..} <- lift $ ICal.runR zones $ ICal.resolveEventOccurrence eo
  let utcEventStart = resolvedEventStart
  let utcEventEnd =
        case resolvedEventEnd of
          Just e -> Just e
          -- Use the event start so we definitely have an endpoint. This is the way google calendar does it.
          -- This is important because otherwise very old events without an end time are always imported.
          Nothing -> utcEventStart
  pure UTCEvent {..}
