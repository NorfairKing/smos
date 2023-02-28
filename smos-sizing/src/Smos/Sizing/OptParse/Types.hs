{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Sizing.OptParse.Types where

import Autodocodec
import Path
import qualified Smos.Report.OptParse.Types as Report

data Arguments
  = Arguments
      !Command
      !(Report.FlagsWithConfigFile Flags)
  deriving (Show, Eq)

data Command
  = CommandReport !ReportFlags
  deriving (Show, Eq)

data ReportFlags = ReportFlags
  { reportFlagPlanFile :: !FilePath
  }
  deriving (Show, Eq)

data Flags = Flags
  deriving (Show, Eq)

data Configuration = Configuration
  { confSizingConfiguration :: !(Maybe SizingConfiguration)
  }
  deriving (Show, Eq)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalFieldOrNull "sizing" "The sizing tool configuration" .= confSizingConfiguration

data SizingConfiguration = SizingConfiguration
  deriving (Show, Eq)

instance HasCodec SizingConfiguration where
  codec = object "SizingConfiguration" $ pure SizingConfiguration

data Environment = Environment
  deriving (Show, Eq)

data Instructions
  = Instructions
      !Dispatch
      !Settings
  deriving (Show, Eq)

data Dispatch
  = DispatchReport ReportSettings
  deriving (Show, Eq)

data ReportSettings = ReportSettings
  { reportSettingPlanFile :: !(Path Abs File)
  }
  deriving (Show, Eq)

data Settings = Settings
  deriving (Show, Eq)
