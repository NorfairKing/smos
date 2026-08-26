{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
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

instance HasCodec ICal.TimeZoneIdentifierParam where
  codec = dimapCodec (ICal.TimeZoneIdentifierParam . ICal.ciToParamValue . CI.mk) (CI.original . ICal.paramValueCI . ICal.unTimeZoneIdentifierParam) codec

deriving via (Autodocodec ICal.TimeZoneIdentifierParam) instance (FromJSON ICal.TimeZoneIdentifierParam)

deriving via (Autodocodec ICal.TimeZoneIdentifierParam) instance (ToJSON ICal.TimeZoneIdentifierParam)

instance FromJSONKey ICal.TimeZoneIdentifierParam where
  fromJSONKey = FromJSONKeyText $ ICal.TimeZoneIdentifierParam . ICal.ciToParamValue . CI.mk

instance ToJSONKey ICal.TimeZoneIdentifierParam where
  toJSONKey = toJSONKeyText $ CI.original . ICal.paramValueCI . ICal.unTimeZoneIdentifierParam

instance HasCodec ICal.TimeZone where
  codec = componentCodec

componentCodec :: (ICal.IsComponent component) => JSONCodec component
componentCodec = bimapCodec to from codec
  where
    to = left show . fmap fst . ICal.runConform . ICal.parseComponentFromText

    from = ICal.renderComponentText

endDurationObjectCodec :: JSONObjectCodec (Maybe (Either ICal.RecurrenceEnd ICal.Duration))
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

-- | A 'RecurrenceEnd' is what a DTEND and a DUE have in common, and is not a
-- property of its own, so it goes through the spelling that a VEVENT uses.
instance HasCodec ICal.RecurrenceEnd where
  codec = dimapCodec ICal.dateTimeEndRecurrenceEnd ICal.recurrenceEndDateTimeEnd (propertyCodec @ICal.DateTimeEnd)

instance HasCodec ICal.RecurrenceIdentifier where
  codec = propertyCodec

instance HasCodec ICal.SequenceNumber where
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

-- | An instance of a recurrence set, without the component it came from
--
-- The component is not part of this: smos groups instances by the component
-- that contributed them and keeps it beside the group, so repeating it on every
-- instance would say nothing.
instance HasCodec (ICal.Occurrence ()) where
  codec =
    object "Occurrence" $
      (\start endOrDuration -> ICal.Occurrence {ICal.occurrenceComponent = (), ICal.occurrenceStart = start, ICal.occurrenceEnd = endOrDuration})
        <$> requiredField "dtstart" "date time start"
          .= ICal.occurrenceStart
        <*> endDurationObjectCodec
          .= ICal.occurrenceEnd

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
