{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module ICal.Extended where

import Autodocodec
import Control.Arrow (left)
import Control.Exception
import Data.Aeson (FromJSON, FromJSONKey (..), FromJSONKeyFunction (..), ToJSON, ToJSONKey (..))
import Data.Aeson.Types (fromJSONKeyCoerce, toJSONKeyText)
import qualified Data.CaseInsensitive as CI
import qualified Data.DList as DList
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Validity
import GHC.Generics
import qualified ICal
import qualified ICal.Component as ICal
import qualified ICal.Conformance as ICal
import qualified ICal.ContentLine as ICal
import qualified ICal.Parameter as ICal
import qualified ICal.Property as ICal
import qualified ICal.PropertyType.RecurrenceRule as ICal
import qualified ICal.Recurrence as ICal
import qualified ICal.UnfoldedLine as ICal
import Smos.Calendar.Import.Static

instance HasCodec ICal.UID where
  codec = dimapCodec ICal.UID ICal.unUID codec

deriving via (Autodocodec ICal.UID) instance (FromJSON ICal.UID)

deriving via (Autodocodec ICal.UID) instance (ToJSON ICal.UID)

instance FromJSONKey ICal.UID where
  fromJSONKey = fromJSONKeyCoerce

instance ToJSONKey ICal.UID where
  toJSONKey = toJSONKeyText ICal.unUID

instance HasCodec ICal.TZID where
  codec = dimapCodec ICal.TZID ICal.unTZID codec

deriving via (Autodocodec ICal.TZID) instance (FromJSON ICal.TZID)

deriving via (Autodocodec ICal.TZID) instance (ToJSON ICal.TZID)

instance FromJSONKey ICal.TZID where
  fromJSONKey = fromJSONKeyCoerce

instance ToJSONKey ICal.TZID where
  toJSONKey = toJSONKeyText ICal.unTZID

instance HasCodec ICal.TZIDParam where
  codec = dimapCodec (ICal.TZIDParam . CI.mk) (CI.original . ICal.unTZIDParam) codec

deriving via (Autodocodec ICal.TZIDParam) instance (FromJSON ICal.TZIDParam)

deriving via (Autodocodec ICal.TZIDParam) instance (ToJSON ICal.TZIDParam)

instance FromJSONKey ICal.TZIDParam where
  fromJSONKey = FromJSONKeyText $ ICal.TZIDParam . CI.mk

instance ToJSONKey ICal.TZIDParam where
  toJSONKey = toJSONKeyText $ CI.original . ICal.unTZIDParam

instance HasCodec ICal.Event where
  codec = componentCodec

deriving via (Autodocodec ICal.Event) instance (FromJSON ICal.Event)

deriving via (Autodocodec ICal.Event) instance (ToJSON ICal.Event)

instance HasCodec ICal.TimeZone where
  codec = componentCodec

deriving via (Autodocodec ICal.TimeZone) instance (FromJSON ICal.TimeZone)

deriving via (Autodocodec ICal.TimeZone) instance (ToJSON ICal.TimeZone)

componentCodec :: (Validity component, ICal.IsComponent component) => JSONCodec component
componentCodec = bimapCodec to from codec
  where
    to = left show . fmap fst . ICal.runConform . ICal.parseComponentFromText

    from = ICal.renderComponentText

instance HasCodec ICal.RecurringEvent where
  codec = object "RecurringEvent" objectCodec

instance HasObjectCodec ICal.RecurringEvent where
  objectCodec =
    ICal.RecurringEvent
      <$> optionalField "dtstart" "start date time" .= ICal.recurringEventStart
      <*> endDurationObjectCodec .= ICal.recurringEventEndOrDuration
      <*> recurrenceObjectCodec .= ICal.recurringEventRecurrence

endDurationObjectCodec :: JSONObjectCodec (Maybe (Either ICal.DateTimeEnd ICal.Duration))
endDurationObjectCodec =
  dimapCodec
    ( \case
        (Nothing, Nothing) -> Nothing
        (Just end, _) -> Just (Left end)
        (Nothing, Just duration) -> Just (Right duration)
    )
    ( \case
        Nothing -> (Nothing, Nothing)
        Just (Left end) -> (Just end, Nothing)
        Just (Right duration) -> (Nothing, Just duration)
    )
    $ (,)
      <$> optionalField "dtend" "end date time" .= fst
      <*> optionalField "duration" "duration" .= snd

recurrenceObjectCodec :: JSONObjectCodec ICal.Recurrence
recurrenceObjectCodec =
  ICal.Recurrence
    <$> optionalFieldWithOmittedDefault "exdate" S.empty "exception date times" .= ICal.recurrenceExceptionDateTimes
    <*> optionalFieldWithOmittedDefault "rdate" S.empty "recurrence date times" .= ICal.recurrenceRecurrenceDateTimes
    <*> optionalFieldWithOmittedDefault "rrule" S.empty "recurrence rules" .= ICal.recurrenceRecurrenceRules

instance HasCodec ICal.ExceptionDateTimes where
  codec = propertyCodec

instance HasCodec ICal.RecurrenceRule where
  codec = propertyCodec

instance HasCodec ICal.RecurrenceDateTimes where
  codec = propertyCodec

instance HasCodec ICal.DateTimeStart where
  codec = propertyCodec

instance HasCodec ICal.DateTimeEnd where
  codec = propertyCodec

instance HasCodec ICal.Duration where
  codec = propertyCodec

propertyCodec :: forall property. (Validity property, ICal.IsProperty property) => JSONCodec property
propertyCodec = bimapCodec from to codec
  where
    from :: Text -> Either String property
    from = left displayException . fmap fst . ICal.runConform . ICal.parsePropertyFromText
    to :: property -> Text
    to = ICal.renderPropertyText

instance HasCodec ICal.EventOccurrence where
  codec =
    object "EventOccurrence" $
      ICal.EventOccurrence
        <$> requiredField "dtstart" "date time start" .= ICal.eventOccurrenceStart
        <*> endDurationObjectCodec .= ICal.eventOccurrenceEndOrDuration