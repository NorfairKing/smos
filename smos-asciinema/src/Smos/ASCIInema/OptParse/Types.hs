{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.ASCIInema.OptParse.Types where

import Path

data Arguments = Arguments Command Flags
  deriving (Show, Eq)

data Command = CommandRecord FilePath
  deriving (Show, Eq)

data Flags
  = Flags
      {
      }
  deriving (Show, Eq)

data Configuration

data Environment
  = Environment
      {
      }
  deriving (Show, Eq)

data Instructions = Instructions Dispatch Settings
  deriving (Show, Eq)

data Dispatch = DispatchRecord (Path Abs File)
  deriving (Show, Eq)

data Settings
  = Settings
      {
      }
  deriving (Show, Eq)
