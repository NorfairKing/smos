{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module ICal.Extended where

import Autodocodec
import Control.Arrow (left)
import Data.Aeson (FromJSON, FromJSONKey (..), ToJSON, ToJSONKey (..))
import Data.Aeson.Types (fromJSONKeyCoerce, toJSONKeyText)
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
import qualified ICal.Property as ICal
import qualified ICal.PropertyType.RecurrenceRule as ICal
import qualified ICal.Recurrence as ICal
import qualified ICal.UnfoldedLine as ICal
import Smos.Calendar.Import.Static

instance HasCodec ICal.TZID where
  codec = dimapCodec ICal.TZID ICal.unTZID codec

deriving via (Autodocodec ICal.TZID) instance (FromJSON ICal.TZID)

deriving via (Autodocodec ICal.TZID) instance (ToJSON ICal.TZID)

instance FromJSONKey ICal.TZID where
  fromJSONKey = fromJSONKeyCoerce

instance ToJSONKey ICal.TZID where
  toJSONKey = toJSONKeyText ICal.unTZID

instance HasCodec ICal.TimeZone where
  codec = bimapCodec to from codec
    where
      to :: Text -> Either String ICal.TimeZone
      to = left show . fmap fst . ICal.runConform . ICal.parseComponentFromText

      from :: ICal.TimeZone -> Text
      from = ICal.renderComponentText

deriving via (Autodocodec ICal.TimeZone) instance (FromJSON ICal.TimeZone)

deriving via (Autodocodec ICal.TimeZone) instance (ToJSON ICal.TimeZone)

instance HasCodec ICal.RecurringEvent where
  codec = undefined -- TODO

instance HasCodec ICal.DateTimeStart where
  codec = undefined -- TODO

instance HasCodec ICal.DateTimeEnd where
  codec = undefined -- TODO

instance HasCodec ICal.Duration where
  codec = undefined -- TODO
