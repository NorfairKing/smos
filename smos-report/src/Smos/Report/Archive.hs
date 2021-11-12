{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Report.Archive where

import Autodocodec
import Data.Aeson
import Data.Validity
import GHC.Generics (Generic)

data HideArchive
  = HideArchive
  | Don'tHideArchive
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec HideArchive)

instance Validity HideArchive

instance HasCodec HideArchive where
  codec =
    dimapCodec
      ( \case
          True -> HideArchive
          False -> Don'tHideArchive
      )
      ( \case
          HideArchive -> True
          Don'tHideArchive -> False
      )
      codec
