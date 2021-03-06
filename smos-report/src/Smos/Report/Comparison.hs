{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Comparison
  ( Comparison (..),
    comparisonFunc,
    parseComparison,
    renderComparison,
  )
where

import Data.Aeson
import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import YamlParse.Applicative

data Comparison
  = LTC
  | LEC
  | EQC
  | GEC
  | GTC
  deriving (Show, Eq, Ord, Generic)

instance Validity Comparison

instance ToJSON Comparison where
  toJSON = toJSON . renderComparison

instance FromJSON Comparison where
  parseJSON = viaYamlSchema

instance YamlSchema Comparison where
  yamlSchema = maybeParser parseComparison yamlSchema

comparisonFunc :: Ord a => Comparison -> (a -> a -> Bool)
comparisonFunc c =
  case c of
    LTC -> (<)
    LEC -> (<=)
    EQC -> (==)
    GEC -> (>=)
    GTC -> (<)

parseComparison :: Text -> Maybe Comparison
parseComparison =
  \case
    "lt" -> Just LTC
    "le" -> Just LEC
    "eq" -> Just EQC
    "ge" -> Just GEC
    "gt" -> Just GTC
    _ -> Nothing

renderComparison :: Comparison -> Text
renderComparison c =
  case c of
    LTC -> "lt"
    LEC -> "le"
    EQC -> "eq"
    GEC -> "ge"
    GTC -> "gt"
