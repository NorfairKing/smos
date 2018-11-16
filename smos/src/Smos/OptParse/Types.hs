{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.OptParse.Types where

import Import

import Smos.Types

data Arguments =
    Arguments FilePath
              Flags

data Flags =
    Flags
    deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data Environment = Environment
    deriving (Show, Eq)

data Instructions =
    Instructions (Path Abs File)
                 SmosConfig
