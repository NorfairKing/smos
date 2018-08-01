{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Draw
    ( smosDraw
    ) where

import Import

import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Time

import Brick.Types as B
import Brick.Widgets.Center as B
import Brick.Widgets.Core as B
import Graphics.Vty.Input.Events (Key(..), Modifier(..))

import Smos.Cursor.SmosFile
import Smos.Data

import Smos.Style
import Smos.Types

smosDraw :: SmosState -> [Widget ResourceName]
smosDraw SmosState {..} = [maybe drawNoContent renderCursor smosStateCursor]
  where
    renderCursor :: SmosFileCursor -> Widget ResourceName
    renderCursor cur =
        drawSmosFileCursor cur <=>
        if smosStateShowDebug
            then B.vBox [drawHistory smosStateKeyHistory, strWrap $ show cur]
            else emptyWidget

drawNoContent :: Widget n
drawNoContent =
    B.vCenterLayer $
    B.vBox $
    map B.hCenterLayer
        [ str "SMOS"
        , str " "
        , str "version 0.0.0"
        , str "by Tom Sydney Kerckhove"
        , str "Smos is open source and freely distributable"
        ]

drawSmosFileCursor :: SmosFileCursor -> Widget ResourceName
drawSmosFileCursor _ = txt "hello"

drawHistory :: [KeyPress] -> Widget n
drawHistory = strWrap . unwords . map showKeypress . reverse
  where
    showKeypress (KeyPress key mods) =
        case mods of
            [] -> showKey key
            _ -> intercalate "-" $ map showMod mods ++ [showKey key]
    showKey (KChar c) = [c]
    showKey (KFun i) = "F" ++ show i
    showKey k = show k
    showMod MShift = "S"
    showMod MCtrl = "C"
    showMod MMeta = "M"
    showMod MAlt = "A"

