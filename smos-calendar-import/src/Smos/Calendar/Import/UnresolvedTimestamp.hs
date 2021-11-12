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
    alternatives
      [ objectParser "period" $ CalPeriodFromTo <$> requiredField' "from" <*> requiredField' "to",
        objectParser "duration" $ CalPeriodDuration <$> requiredField' "from" <*> requiredField' "duration"
      ]

data CalEndDuration
  = CalTimestamp CalTimestamp
  | CalDuration Int -- Seconds
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec CalEndDuration)

instance Validity CalEndDuration

instance HasCodec CalEndDuration where
  codec =
    alternatives
      [ CalTimestamp <$> codec,
        CalDuration <$> codec
      ]

data CalTimestamp
  = CalDate Day
  | CalDateTime CalDateTime
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec CalTimestamp)

instance Validity CalTimestamp

instance HasCodec CalTimestamp where
  codec =
    alternatives
      [ CalDate <$> daySchema,
        CalDateTime <$> codec
      ]

-- https://tools.ietf.org/html/rfc5545#section-3.3.5
data CalDateTime
  = Floating LocalTime
  | UTC UTCTime
  | Zoned LocalTime TimeZoneId
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec CalDateTime)

instance Validity CalDateTime

instance HasCodec CalDateTime where
  codec =
    alternatives
      [ objectParser "CalDateTime" $
          alternatives
            [ Floating <$> requiredFieldWith' "floating" localTimeSchema,
              UTC <$> requiredFieldWith' "utc" (localTimeToUTC utc <$> localTimeSchema),
              Zoned <$> requiredFieldWith' "local" localTimeSchema <*> requiredField' "zone"
            ],
        Floating <$> localTimeSchema
      ]

newtype TimeZoneId = TimeZoneId Text -- Unique id of the timezone
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, FromJSONKey, ToJSON, ToJSONKey, IsString)

instance Validity TimeZoneId

instance HasCodec TimeZoneId where
  codec = TimeZoneId <$> codec
