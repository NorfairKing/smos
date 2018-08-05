{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Draw
    ( smosDraw
    ) where

import Import hiding ((<+>))

-- import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

-- import qualified Data.Text as T
-- import Data.Time
import Brick.Types as B
import Brick.Widgets.Center as B
import Brick.Widgets.Core as B
import Brick.Widgets.Core ((<+>))
import Graphics.Vty.Input.Events (Key(..), Modifier(..))

import Cursor.Forest
import Cursor.NonEmpty
import Cursor.Tree

import Smos.Cursor.SmosFile

-- import Smos.Data
-- import Smos.Style
import Smos.Style
import Smos.Types

smosDraw :: SmosConfig -> SmosState -> [Widget ResourceName]
smosDraw SmosConfig {..} SmosState {..} =
    [maybe drawNoContent renderCursor smosStateCursor]
  where
    renderCursor :: SmosFileCursor -> Widget ResourceName
    renderCursor cur =
        drawSmosFileCursor cur <=>
        if smosStateShowDebug
            then B.vBox [drawHistory smosStateKeyHistory, strWrap $ show cur]
            else emptyWidget
    drawNoContent :: Widget n
    drawNoContent = B.vCenterLayer $ B.vBox [drawInfo, drawHelpPage]
      where
        drawInfo :: Widget n
        drawInfo =
            withAttr selectedAttr $
            vBox $
            map
                B.hCenterLayer
                [ str "SMOS"
                , str " "
                , str "version 0.0.0"
                , str "by Tom Sydney Kerckhove"
                , str "Smos is open source and freely distributable"
                , str " "
                , str " "
                ]
        drawHelpPage :: Widget n
        drawHelpPage =
            vBox $
            flip map maps $ \(n, m) ->
                vBox
                    [ withAttr selectedAttr $ hCenter $ str n
                    , hCenter $
                      drawTable $
                      flip map (M.toList $ m configKeyMap) $ \(km, a) ->
                          (drawKeyMatch km, txt (actionName a))
                    ]
          where
            maps :: [(String, KeyMap -> Map KeyMatch Action)]
            maps = [("Empty file", keyMapEmptyMatchers)]
            drawKeyMatch :: KeyMatch -> Widget n
            drawKeyMatch (MatchExactly k mods) =
                str $ showKeypress (KeyPress k mods)

drawSmosFileCursor :: SmosFileCursor -> Widget ResourceName
drawSmosFileCursor =
    drawVerticalForestCursor (strWrap . show) (withAttr selectedAttr . strWrap . show) (strWrap . show)

drawHistory :: [KeyPress] -> Widget n
drawHistory = strWrap . unwords . map showKeypress . reverse

drawTable :: [(Widget n, Widget n)] -> Widget n
drawTable ls = vBox (map fst ls) <+> str "   " <+> vBox (map snd ls)

showKeypress :: KeyPress -> String
showKeypress (KeyPress key mods) =
    case mods of
        [] -> showKey key
        _ -> intercalate "-" $ map showMod mods ++ [showKey key]

showKey :: Key -> String
showKey (KChar c) = [c]
showKey (KFun i) = "F" ++ show i
showKey (KEsc) = "Esc"
showKey k = show k

showMod :: Modifier -> String
showMod MShift = "S"
showMod MCtrl = "C"
showMod MMeta = "M"
showMod MAlt = "A"

drawVerticalForestCursor ::
       (TreeCursor a -> Widget n)
    -> (TreeCursor a -> Widget n)
    -> (TreeCursor a -> Widget n)
    -> ForestCursor a
    -> Widget n
drawVerticalForestCursor prevFunc curFunc nextFunc =
    drawForestCursor
        prevFunc
        curFunc
        nextFunc
        B.vBox
        B.vBox
        (\a b c -> a <=> b <=> c)

drawForestCursor ::
       (TreeCursor a -> Widget n)
    -> (TreeCursor a -> Widget n)
    -> (TreeCursor a -> Widget n)
    -> ([Widget n] -> Widget n)
    -> ([Widget n] -> Widget n)
    -> (Widget n -> Widget n -> Widget n -> Widget n)
    -> ForestCursor a
    -> Widget n
drawForestCursor prevFunc curFunc nextFunc prevCombFunc nextCombFunc combFunc fc =
    drawNonEmptyCursor
        prevFunc
        curFunc
        nextFunc
        prevCombFunc
        nextCombFunc
        combFunc $
    forestCursorListCursor fc

drawVerticalNonEmptyCursor ::
       (a -> Widget n)
    -> (a -> Widget n)
    -> (a -> Widget n)
    -> NonEmptyCursor a
    -> Widget n
drawVerticalNonEmptyCursor prevFunc curFunc nextFunc =
    drawNonEmptyCursor
        prevFunc
        curFunc
        nextFunc
        B.vBox
        B.vBox
        (\a b c -> a <=> b <=> c)

drawNonEmptyCursor ::
       (a -> Widget n)
    -> (a -> Widget n)
    -> (a -> Widget n)
    -> ([Widget n] -> Widget n)
    -> ([Widget n] -> Widget n)
    -> (Widget n -> Widget n -> Widget n -> Widget n)
    -> NonEmptyCursor a
    -> Widget n
drawNonEmptyCursor prevFunc curFunc nextFunc prevCombFunc nextCombFunc combFunc NonEmptyCursor {..} =
    let prev = prevCombFunc $ map prevFunc $ reverse nonEmptyCursorPrev
        cur = curFunc nonEmptyCursorCurrent
        next = nextCombFunc $ map nextFunc nonEmptyCursorNext
     in combFunc prev cur next
