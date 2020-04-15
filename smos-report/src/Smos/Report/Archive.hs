{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Archive where

import Data.Aeson
import Data.Validity
import GHC.Generics (Generic)

data HideArchive
  = HideArchive
  | Don'tHideArchive
  deriving (Show, Eq, Generic)

instance Validity HideArchive

instance FromJSON HideArchive where
  parseJSON =
    withBool "HideArchive" $ \b ->
      pure $
        if b
          then HideArchive
          else Don'tHideArchive

instance ToJSON HideArchive where
  toJSON HideArchive = Bool True
  toJSON Don'tHideArchive = Bool False
