{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ICal.Extended where

import Autodocodec
import Control.Arrow (left)
import Control.Exception
import Data.Aeson (FromJSON, FromJSONKey (..), FromJSONKeyFunction (..), ToJSON, ToJSONKey (..))
import Data.Aeson.Types (fromJSONKeyCoerce, toJSONKeyText)
import qualified Data.CaseInsensitive as CI
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time
import qualified ICal
import qualified ICal.Recurrence as ICal

instance HasCodec ICal.UID where
  codec = dimapCodec ICal.UID ICal.unUID codec

deriving via (Autodocodec ICal.UID) instance (FromJSON ICal.UID)

deriving via (Autodocodec ICal.UID) instance (ToJSON ICal.UID)

instance FromJSONKey ICal.UID where
  fromJSONKey = fromJSONKeyCoerce

instance ToJSONKey ICal.UID where
  toJSONKey = toJSONKeyText ICal.unUID

instance HasCodec ICal.TimeZoneIdentifier where
  codec = dimapCodec ICal.TimeZoneIdentifier ICal.unTimeZoneIdentifier codec

deriving via (Autodocodec ICal.TimeZoneIdentifier) instance (FromJSON ICal.TimeZoneIdentifier)

deriving via (Autodocodec ICal.TimeZoneIdentifier) instance (ToJSON ICal.TimeZoneIdentifier)

instance FromJSONKey ICal.TimeZoneIdentifier where
  fromJSONKey = fromJSONKeyCoerce

instance ToJSONKey ICal.TimeZoneIdentifier where
  toJSONKey = toJSONKeyText ICal.unTimeZoneIdentifier

instance HasCodec ICal.TimeZoneIdentifierParam where
  codec = dimapCodec (ICal.TimeZoneIdentifierParam . CI.mk) (CI.original . ICal.unTimeZoneIdentifierParam) codec

deriving via (Autodocodec ICal.TimeZoneIdentifierParam) instance (FromJSON ICal.TimeZoneIdentifierParam)

deriving via (Autodocodec ICal.TimeZoneIdentifierParam) instance (ToJSON ICal.TimeZoneIdentifierParam)

instance FromJSONKey ICal.TimeZoneIdentifierParam where
  fromJSONKey = FromJSONKeyText $ ICal.TimeZoneIdentifierParam . CI.mk

instance ToJSONKey ICal.TimeZoneIdentifierParam where
  toJSONKey = toJSONKeyText $ CI.original . ICal.unTimeZoneIdentifierParam

instance HasCodec ICal.Event where
  codec = componentCodec

deriving via (Autodocodec ICal.Event) instance (FromJSON ICal.Event)

deriving via (Autodocodec ICal.Event) instance (ToJSON ICal.Event)

instance HasCodec ICal.TimeZone where
  codec = componentCodec

deriving via (Autodocodec ICal.TimeZone) instance (FromJSON ICal.TimeZone)

deriving via (Autodocodec ICal.TimeZone) instance (ToJSON ICal.TimeZone)

componentCodec :: (ICal.IsComponent component) => JSONCodec component
componentCodec = bimapCodec to from codec
  where
    to = left show . fmap fst . ICal.runConform . ICal.parseComponentFromText

    from = ICal.renderComponentText

instance HasCodec ICal.RecurringEvent where
  codec = object "RecurringEvent" objectCodec

instance HasObjectCodec ICal.RecurringEvent where
  objectCodec =
    ICal.RecurringEvent
      <$> optionalField "dtstart" "start date time"
        .= ICal.recurringEventStart
      <*> endDurationObjectCodec
        .= ICal.recurringEventEndOrDuration
      <*> recurrenceObjectCodec
        .= ICal.recurringEventRecurrence

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
      <$> optionalField "dtend" "end date time"
        .= fst
      <*> optionalField "duration" "duration"
        .= snd

recurrenceObjectCodec :: JSONObjectCodec ICal.Recurrence
recurrenceObjectCodec =
  ICal.Recurrence
    <$> optionalFieldWithOmittedDefault "exdate" S.empty "exception date times"
      .= ICal.recurrenceExceptionDateTimes
    <*> optionalFieldWithOmittedDefault "rdate" S.empty "recurrence date times"
      .= ICal.recurrenceRecurrenceDateTimes
    <*> optionalFieldWithOmittedDefault "rrule" S.empty "recurrence rules"
      .= ICal.recurrenceRecurrenceRules

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

propertyCodec :: forall property. (ICal.IsProperty property) => JSONCodec property
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
        <$> requiredField "dtstart" "date time start"
          .= ICal.eventOccurrenceStart
        <*> endDurationObjectCodec
          .= ICal.eventOccurrenceEndOrDuration

instance HasCodec ICal.Timestamp where
  codec = dimapCodec f g $ eitherCodec dayCodec (eitherCodec localTimeCodec utctimeCodec)
    where
      f = \case
        Left d -> ICal.TimestampDay d
        Right (Left lt) -> ICal.TimestampLocalTime lt
        Right (Right ut) -> ICal.TimestampUTCTime ut
      g = \case
        ICal.TimestampDay d -> Left d
        ICal.TimestampLocalTime lt -> Right (Left lt)
        ICal.TimestampUTCTime ut -> Right (Right ut)
      dayCodec = timeCodec "Day %F"
      localTimeCodec = timeCodec "Local %F %T"
      utctimeCodec = timeCodec "UTC %F %T"
      timeCodec format =
        bimapCodec
          ( \s -> case parseTimeM True defaultTimeLocale format s of
              Nothing -> Left "Could not parse time."
              Just t -> Right t
          )
          (formatTime defaultTimeLocale format)
          codec
