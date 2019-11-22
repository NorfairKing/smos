{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Comparison where

import GHC.Generics (Generic)

import Data.Text (Text)
import Data.Validity

data Comparison
  = LTC
  | LEC
  | EQC
  | GEC
  | GTC
  deriving (Show, Eq, Ord, Generic)

instance Validity Comparison

comparisonFunc :: Ord a => Comparison -> (a -> a -> Bool)
comparisonFunc c =
  case c of
    LTC -> (<)
    LEC -> (<=)
    EQC -> (==)
    GEC -> (>=)
    GTC -> (<)

renderComparison :: Comparison -> Text
renderComparison c =
  case c of
    LTC -> "lt"
    LEC -> "le"
    EQC -> "eq"
    GEC -> "ge"
    GTC -> "gt"
