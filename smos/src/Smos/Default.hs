{-# LANGUAGE OverloadedStrings #-}

module Smos.Default where

import Import

import Smos
import Smos.Style
import Smos.Types

defaultSmos :: IO ()
defaultSmos = smos defaultConfig

defaultConfig :: SmosConfig
defaultConfig =
    SmosConfig
        { configKeyMap = KeyMap
        }
