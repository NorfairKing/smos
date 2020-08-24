{-# LANGUAGE TemplateHaskell #-}

module Smos.Version where

import Smos.Version.TH

smosVersion :: String
smosVersion = $mkVersionInfo
