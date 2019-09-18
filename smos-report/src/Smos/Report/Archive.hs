{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Archive where

import GHC.Generics (Generic)

import Data.Aeson

data HideArchive
  = HideArchive
  | Don'tHideArchive
  deriving (Show, Eq, Generic)

instance FromJSON HideArchive where
  parseJSON =
    withBool "HideArchive" $ \b ->
      pure $
      if b
        then HideArchive
        else Don'tHideArchive
