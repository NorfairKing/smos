{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Draw
    ( smosDraw
    ) where

import Import hiding ((<+>))

import qualified Data.Map as M

import Brick.Types as B
import Brick.Widgets.Border as B
import Brick.Widgets.Center as B
import Brick.Widgets.Core as B
import Brick.Widgets.Core ((<+>))
import Graphics.Vty.Input.Events (Key(..), Modifier(..))

import Lens.Micro

import Data.Tree

import Cursor.Text
import Cursor.TextField
import Cursor.Tree

import Smos.Cursor.Contents
import Smos.Cursor.Entry
import Smos.Cursor.Header
import Smos.Cursor.Logbook
import Smos.Cursor.Properties
import Smos.Cursor.SmosFile
import Smos.Cursor.StateHistory
import Smos.Cursor.Tags
import Smos.Cursor.Timestamps

import Smos.Draw.Cursor
import Smos.Style
import Smos.Types

smosDraw :: SmosConfig -> SmosState -> [Widget ResourceName]
smosDraw SmosConfig {..} SmosState {..} =
    [centerLayer drawContextualHelpPage | smosStateShowHelp] ++
    [maybe drawNoContent renderCursor smosStateCursor]
  where
    renderCursor :: SmosFileCursor -> Widget ResourceName
    renderCursor cur =
        drawSmosFileCursor cur <=>
        if smosStateShowDebug
            then B.vBox [drawHistory smosStateKeyHistory, strWrap $ show cur]
            else emptyWidget
    drawNoContent :: Widget n
    drawNoContent = B.vCenterLayer $ B.vBox [drawInfo, drawEmptyHelpPage]
      where
        drawEmptyHelpPage :: Widget n
        drawEmptyHelpPage =
            vBox $
            map
                (\(n, km) ->
                     vBox
                         [ hCenterLayer $ withAttr selectedAttr $ str n
                         , drawKeyMapHelp km
                         ])
                keyMaps
    drawContextualHelpPage :: Widget n
    drawContextualHelpPage =
        case smosStateCursor of
            Nothing -> pageFor "Empty file" keyMapEmptyMatchers
            Just sfc ->
                case sfc ^. smosFileCursorEntrySelectionL of
                    WholeEntrySelected -> pageFor "Entry" keyMapEntryMatchers
                    HeaderSelected -> pageFor "Header" keyMapHeaderMatchers
                    ContentsSelected ->
                        pageFor "Contents" keyMapContentsMatchers
                    TimestampsSelected ->
                        pageFor "Timestamps" keyMapTimestampsMatchers
                    PropertiesSelected ->
                        pageFor "Properties" keyMapPropertiesMatchers
                    StateHistorySelected ->
                        pageFor "State History" keyMapStateHistoryMatchers
                    TagsSelected -> pageFor "Tags" keyMapTagsMatchers
                    LogbookSelected -> pageFor "Logbook" keyMapLogbookMatchers
      where
        pageFor s bindings =
            borderWithLabel (withAttr selectedAttr $ str ("[" ++ s ++ "]")) $
            padAll 1 $ drawKeyMapHelp bindings
    drawKeyMapHelp :: (KeyMap -> Map KeyMatch Action) -> Widget n
    drawKeyMapHelp m =
        padBottom (Pad 1) $
        hCenterLayer $
        drawTable $
        flip map (M.toAscList $ m configKeyMap) $ \(km, a) ->
            (drawKeyMatch km, txt (actionName a))
      where
        drawKeyMatch :: KeyMatch -> Widget n
        drawKeyMatch (MatchExactly k mods) =
            str $ showKeypress (KeyPress k mods)
    keyMaps :: [(String, KeyMap -> Map KeyMatch Action)]
    keyMaps =
        [ ("Empty file", keyMapEmptyMatchers)
        , ("Entry", keyMapEntryMatchers)
        , ("Header", keyMapHeaderMatchers)
        , ("Contents", keyMapContentsMatchers)
        , ("Timestamps", keyMapTimestampsMatchers)
        , ("Properties", keyMapPropertiesMatchers)
        , ("State History", keyMapStateHistoryMatchers)
        , ("Tags", keyMapTagsMatchers)
        , ("Logbook", keyMapLogbookMatchers)
        ]
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

data Select
    = MaybeSelected
    | NotSelected

defaultPadding :: Padding
defaultPadding = Pad 2

drawSmosFileCursor :: SmosFileCursor -> Widget ResourceName
drawSmosFileCursor =
    drawVerticalForestCursor
        (drawSmosTreeCursor NotSelected)
        (drawSmosTreeCursor MaybeSelected)
        (drawSmosTreeCursor NotSelected)

drawSmosTreeCursor :: Select -> TreeCursor EntryCursor -> Widget ResourceName
drawSmosTreeCursor s = drawTreeCursor wrap cur
  where
    cur :: EntryCursor -> Forest EntryCursor -> Widget ResourceName
    cur ec ts =
        drawEntryCursor s ec <=>
        padLeft defaultPadding (vBox $ map drawEntryTree ts)
    wrap ::
           [Tree EntryCursor]
        -> EntryCursor
        -> [Tree EntryCursor]
        -> Widget ResourceName
        -> Widget ResourceName
    wrap tsl e tsr w =
        drawEntryCursor NotSelected e <=>
        padLeft
            defaultPadding
            (vBox $ concat [map drawEntryTree tsl, [w], map drawEntryTree tsr])
    drawEntryTree :: Tree EntryCursor -> Widget ResourceName
    drawEntryTree (Node t ts) =
        drawEntryCursor NotSelected t <=>
        padLeft defaultPadding (vBox $ map drawEntryTree ts)

drawEntryCursor :: Select -> EntryCursor -> Widget ResourceName
drawEntryCursor s EntryCursor {..} =
    (case (s, entryCursorSelected) of
         (MaybeSelected, WholeEntrySelected) -> withAttr selectedAttr
         _ -> id) $
    vBox
        [ drawHeaderCursor (selectWhen HeaderSelected) entryCursorHeaderCursor
        , maybe
              emptyWidget
              (drawContentsCursor $ selectWhen ContentsSelected)
              entryCursorContentsCursor
        , maybe
              emptyWidget
              (drawTimestampsCursor $ selectWhen TimestampsSelected)
              entryCursorTimestampsCursor
        , maybe
              emptyWidget
              (drawPropertiesCursor $ selectWhen PropertiesSelected)
              entryCursorPropertiesCursor
        , maybe
              emptyWidget
              (drawStateHistoryCursor $ selectWhen StateHistorySelected)
              entryCursorStateHistoryCursor
        , maybe
              emptyWidget
              (drawTagsCursor $ selectWhen TagsSelected)
              entryCursorTagsCursor
        , drawLogbookCursor
              (selectWhen LogbookSelected)
              entryCursorLogbookCursor
        ]
  where
    selectWhen :: EntryCursorSelection -> Select
    selectWhen ecs =
        if ecs == entryCursorSelected
            then s
            else NotSelected

drawHeaderCursor :: Select -> HeaderCursor -> Widget ResourceName
drawHeaderCursor = drawTextCursor

drawContentsCursor :: Select -> ContentsCursor -> Widget ResourceName
drawContentsCursor = drawTextFieldCursor

drawTimestampsCursor :: Select -> TimestampsCursor -> Widget ResourceName
drawTimestampsCursor _ = strWrap . show

drawPropertiesCursor :: Select -> PropertiesCursor -> Widget ResourceName
drawPropertiesCursor _ = strWrap . show

drawStateHistoryCursor :: Select -> StateHistoryCursor -> Widget ResourceName
drawStateHistoryCursor _ = strWrap . show

drawTagsCursor :: Select -> TagsCursor -> Widget ResourceName
drawTagsCursor _ = strWrap . show

drawLogbookCursor :: Select -> LogbookCursor -> Widget ResourceName
drawLogbookCursor _ lbc = case lbc of
    LogbookCursorClosed Nothing -> emptyWidget
    _ -> strWrap $ show lbc

drawTextCursor :: Select -> TextCursor -> Widget ResourceName
drawTextCursor _ = strWrap . show

drawTextFieldCursor :: Select -> TextFieldCursor -> Widget ResourceName
drawTextFieldCursor _ = strWrap . show
