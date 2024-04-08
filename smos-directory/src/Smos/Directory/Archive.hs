{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Directory.Archive where

import Autodocodec
import Data.Validity
import GHC.Generics (Generic)

data HideArchive
  = HideArchive
  | Don'tHideArchive
  deriving (Show, Eq, Generic)

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
