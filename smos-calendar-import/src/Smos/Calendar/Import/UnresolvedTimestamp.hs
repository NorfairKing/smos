{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.Import.UnresolvedTimestamp where

import Autodocodec
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.String
import Data.Text (Text)
import Data.Time
import Data.Validity
import GHC.Generics (Generic)
import Smos.Data

data CalRDate
  = CalRTimestamp CalTimestamp
  | CalRPeriod CalPeriod
  deriving (Show, Eq, Ord, Generic)

instance Validity CalRDate

instance HasCodec CalRDate where
  codec = dimapCodec f g $ eitherCodec codec codec
    where
      f = \case
        Left cts -> CalRTimestamp cts
        Right cp -> CalRPeriod cp
      g = \case
        CalRTimestamp cts -> Left cts
        CalRPeriod cp -> Right cp

data CalPeriod
  = CalPeriodFromTo CalDateTime CalDateTime
  | CalPeriodDuration CalDateTime Int -- Seconds
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec CalPeriod)

instance Validity CalPeriod

instance HasCodec CalPeriod where
  codec =
    dimapCodec f g $
      eitherCodec
        ( object "period" $
            (,)
              <$> requiredField' "from" .= fst
              <*> requiredField' "to" .= snd
        )
        ( object "duration" $
            (,)
              <$> requiredField' "from" .= fst
              <*> requiredField' "duration" .= snd
        )
    where
      f = \case
        Left (from, to) -> CalPeriodFromTo from to
        Right (from, duration) -> CalPeriodDuration from duration
      g = \case
        CalPeriodFromTo from to -> Left (from, to)
        CalPeriodDuration from duration -> Right (from, duration)

data CalEndDuration
  = CalTimestamp CalTimestamp
  | CalDuration Int -- Seconds
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec CalEndDuration)

instance Validity CalEndDuration

instance HasCodec CalEndDuration where
  codec = dimapCodec f g $ eitherCodec codec codec
    where
      f = \case
        Left cts -> CalTimestamp cts
        Right i -> CalDuration i
      g = \case
        CalTimestamp cts -> Left cts
        CalDuration i -> Right i

data CalTimestamp
  = CalDate Day
  | CalDateTime CalDateTime
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec CalTimestamp)

instance Validity CalTimestamp

instance HasCodec CalTimestamp where
  codec = dimapCodec f g $ eitherCodec codec codec
    where
      f = \case
        Left d -> CalDate d
        Right dt -> CalDateTime dt
      g = \case
        CalDate d -> Left d
        CalDateTime dt -> Right dt

-- https://tools.ietf.org/html/rfc5545#section-3.3.5
data CalDateTime
  = Floating LocalTime
  | UTC UTCTime
  | Zoned LocalTime TimeZoneId
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec CalDateTime)

instance Validity CalDateTime

instance HasCodec CalDateTime where
  codec = dimapCodec f1 g1 $ eitherCodec codec viaObjectCodec
    where
      f1 = \case
        Left lt -> Floating lt
        Right ctd -> ctd
      g1 = \case
        Floating f -> Left f
        cdt -> Right cdt
      viaObjectCodec =
        dimapCodec
          f2
          g2
          ( eitherCodec
              (object "Floating" $ requiredFieldWith' "floating" localTimeCodec)
              ( eitherCodec
                  (object "UTC" $ requiredFieldWith' "utc" utcViaLocalTimeCodec)
                  ( object "Zoned" $
                      (,)
                        <$> requiredFieldWith' "local" localTimeCodec .= fst
                        <*> requiredField' "zone" .= snd
                  )
              )
          )
      utcViaLocalTimeCodec :: JSONCodec UTCTime
      utcViaLocalTimeCodec = dimapCodec (localTimeToUTC utc) (utcToLocalTime utc) localTimeCodec
      f2 = \case
        Left lt -> Floating lt
        Right (Left u) -> UTC u
        Right (Right (lt, tzid)) -> Zoned lt tzid
      g2 = \case
        Floating lt -> Left lt
        UTC u -> Right (Left u)
        Zoned lt tzid -> Right (Right (lt, tzid))

-- alternatives
--   [ object "CalDateTime" $
--       alternatives
--         [ Floating <$> requiredFieldWith' "floating" localTimeSchema,
--           UTC <$> requiredFieldWith' "utc" (localTimeToUTC utc <$> localTimeSchema),
--           Zoned <$> requiredFieldWith' "local" localTimeSchema <*> requiredField' "zone"
--         ],
--     Floating <$> localTimeSchema
--   ]

newtype TimeZoneId = TimeZoneId {unTimeZoneId :: Text} -- Unique id of the timezone
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, FromJSONKey, ToJSON, ToJSONKey, IsString)

instance Validity TimeZoneId

instance HasCodec TimeZoneId where
  codec = dimapCodec TimeZoneId unTimeZoneId codec
