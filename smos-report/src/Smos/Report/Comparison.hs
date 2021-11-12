{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Comparison
  ( Comparison (..),
    comparisonFunc,
    parseComparison,
    renderComparison,
  )
where

import Autodocodec
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import GHC.Generics (Generic)

data Comparison
  = LTC
  | LEC
  | EQC
  | GEC
  | GTC
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Comparison)

instance Validity Comparison

instance HasCodec Comparison where
  codec = bimapCodec parseComparison renderComparison codec

comparisonFunc :: Ord a => Comparison -> (a -> a -> Bool)
comparisonFunc c =
  case c of
    LTC -> (<)
    LEC -> (<=)
    EQC -> (==)
    GEC -> (>=)
    GTC -> (<)

parseComparison :: Text -> Either String Comparison
parseComparison c =
  case c of
    "lt" -> Right LTC
    "le" -> Right LEC
    "eq" -> Right EQC
    "ge" -> Right GEC
    "gt" -> Right GTC
    _ -> Left $ "Unknown comparison: " <> T.unpack c

renderComparison :: Comparison -> Text
renderComparison c =
  case c of
    LTC -> "lt"
    LEC -> "le"
    EQC -> "eq"
    GEC -> "ge"
    GTC -> "gt"
