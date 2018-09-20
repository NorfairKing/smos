{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Draw
    ( smosDraw
    ) where

import Import hiding ((<+>))

import qualified Data.List.NonEmpty as NE
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
import Cursor.Tree hiding (drawTreeCursor)

import Smos.Data

import Smos.Cursor.Collapse
import Smos.Cursor.Contents
import Smos.Cursor.Editor
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
smosDraw SmosConfig {..} ss@SmosState {..} =
    [ centerLayer drawContextualHelpPage
    | editorCursorSelection smosStateCursor == HelpSelected
    ] ++
    [ vBox $
      concat
          [ [ maybe drawNoContent renderCursor $
              editorCursorFileCursor smosStateCursor
            ]
          , [drawDebug ss | editorCursorDebug smosStateCursor]
          ]
    ]
  where
    renderCursor :: SmosFileCursor -> Widget ResourceName
    renderCursor = drawSmosFileCursor
    drawNoContent :: Widget n
    drawNoContent = B.vCenterLayer $ B.vBox [drawInfo, drawEmptyHelpPage]
      where
        drawEmptyHelpPage :: Widget n
        drawEmptyHelpPage =
            vBox $
            map
                (\(n, km) ->
                     padBottom (Pad 1) $
                     vBox
                         [ hCenterLayer $ withAttr selectedAttr $ str n
                         , hCenterLayer $ drawKeyMapHelp km
                         ])
                keyMaps
    drawContextualHelpPage :: Widget n
    drawContextualHelpPage =
        case smosStateCursor ^. editorCursorSmosFileCursorL of
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
        pageFor :: String -> (KeyMap -> KeyMappings) -> Widget n
        pageFor s bindings =
            borderWithLabel (withAttr selectedAttr $ str ("[" ++ s ++ "]")) $
            padAll 1 $ drawKeyMapHelp bindings
    drawKeyMapHelp :: (KeyMap -> KeyMappings) -> Widget n
    drawKeyMapHelp m =
        drawTable $
        flip map (m configKeyMap) $ \km ->
            (drawKeyMappingEvent km, txt (keyMappingActionName km))
    keyMaps :: [(String, KeyMap -> KeyMappings)]
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
        , ("Help", keyMapHelpMatchers)
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

drawKeyMappingEvent :: KeyMapping -> Widget n
drawKeyMappingEvent (MapVtyExactly kp _) = str $ showKeypress kp
drawKeyMappingEvent (MapCatchAll _) = str "<any key>"
drawKeyMappingEvent (MapAnyTypeableChar _) = str "<any char>"
drawKeyMappingEvent (MapCombination kp km) =
    hBox [str $ showKeypress kp, drawKeyMappingEvent km]

keyMappingActionName :: KeyMapping -> Text
keyMappingActionName (MapVtyExactly _ a) = actionName a
keyMappingActionName (MapCatchAll a) = actionName a
keyMappingActionName (MapAnyTypeableChar a) = actionUsingName a
keyMappingActionName (MapCombination _ km) = keyMappingActionName km

drawHistory :: Seq KeyPress -> Widget n
drawHistory = strWrap . unwords . map showKeypress . toList

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

drawDebug :: SmosState -> Widget n
drawDebug SmosState {..} =
    vBox
        [ str "Key history: " <+> drawHistory smosStateKeyHistory
        , str "Last match: " <+>
          drawLastMatches (debugInfoLastMatches smosStateDebugInfo)
        , strWrap $ ppShow smosStateCursor
        ]

drawLastMatches :: Maybe (NonEmpty ActivationDebug) -> Widget n
drawLastMatches Nothing = emptyWidget
drawLastMatches (Just ts) = vBox $ map (strWrap . ppShow) $ NE.toList ts

data Select
    = MaybeSelected
    | NotSelected

instance Semigroup Select where
    MaybeSelected <> MaybeSelected = MaybeSelected
    _ <> _ = NotSelected

defaultPadding :: Padding
defaultPadding = Pad 2

drawSmosFileCursor :: SmosFileCursor -> Widget ResourceName
drawSmosFileCursor =
    drawVerticalForestCursor drawEntryTree drawSmosTreeCursor drawEntryTree

drawSmosTreeCursor ::
       TreeCursor (Collapse EntryCursor) (Collapse Entry) -> Widget ResourceName
drawSmosTreeCursor = drawTreeCursor wrap cur
  where
    cur :: Collapse EntryCursor
        -> Forest (Collapse Entry)
        -> Widget ResourceName
    cur ec ts =
        drawEntryCursor (collapseTreeValue ec) &
        (if collapseTreeShowSubForest ec
             then (<=> padLeft defaultPadding (vBox $ map drawEntryTree ts))
             else if null ts
                      then id
                      else (<+> str " +++"))
    wrap ::
           [Tree (Collapse Entry)]
        -> Collapse Entry
        -> [Tree (Collapse Entry)]
        -> Widget ResourceName
        -> Widget ResourceName
    wrap tsl e tsr w =
        drawEntry (collapseTreeValue e) &
        if collapseTreeShowSubForest e
            then (<=> padLeft
                          defaultPadding
                          (vBox $
                           concat
                               [ map drawEntryTree tsl
                               , [w]
                               , map drawEntryTree tsr
                               ]))
            else id

drawEntryTree :: Tree (Collapse Entry) -> Widget ResourceName
drawEntryTree (Node t ts) =
    drawEntry (collapseTreeValue t) &
    if collapseTreeShowSubForest t
        then (<=> padLeft defaultPadding (vBox $ map drawEntryTree ts))
        else if null ts
                 then id
                 else (<+> str " +++")

drawEntryCursor :: CollapseEntry EntryCursor -> Widget ResourceName
drawEntryCursor e =
    vBox
        [ hBox
              [ (case selectWhen WholeEntrySelected of
                     MaybeSelected -> withAttr selectedAttr
                     NotSelected -> id) $
                str "> "
              , maybe
                    emptyWidget
                    drawCurrentStateFromCursor
                    entryCursorStateHistoryCursor
              , drawHeaderCursor
                    (selectWhen HeaderSelected)
                    entryCursorHeaderCursor
              , let e_ = rebuildEntryCursor ec
                    anythingHiddenBelow =
                        or
                            [ not (collapseEntryShowContents e) &&
                              not (maybe False nullContents $ entryContents e_)
                            , not (collapseEntryShowHistory e) &&
                              not (nullStateHistory $ entryStateHistory e_)
                            ]
                 in if anythingHiddenBelow
                        then str " ..."
                        else emptyWidget
              ]
        , drawIf collapseEntryShowContents $
          maybe
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
        , drawIf collapseEntryShowHistory $
          maybe
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
    ec@EntryCursor {..} = collapseEntryValue e
    drawIf bf w =
        if bf e
            then w
            else emptyWidget
    selectWhen :: EntryCursorSelection -> Select
    selectWhen ecs =
        if ecs == entryCursorSelected
            then MaybeSelected
            else NotSelected

drawEntry :: CollapseEntry Entry -> Widget ResourceName
drawEntry e =
    vBox
        [ hBox
              [ str "> "
              , drawCurrentState entryStateHistory
              , drawHeader entryHeader
              , let anythingHiddenBelow =
                        or
                            [ not (collapseEntryShowContents e) &&
                              not (maybe False nullContents entryContents)
                            , not (collapseEntryShowHistory e) &&
                              not (nullStateHistory entryStateHistory)
                            ]
                 in if anythingHiddenBelow
                        then str " ..."
                        else emptyWidget
              ]
        , drawIf collapseEntryShowContents $
          maybe emptyWidget drawContents entryContents
        , drawTimestamps entryTimestamps
        , drawProperties entryProperties
        , drawIf collapseEntryShowHistory $ drawStateHistory entryStateHistory
        , drawTags entryTags
        , drawLogbook entryLogbook
        ]
  where
    Entry {..} = collapseEntryValue e
    drawIf bf w =
        if bf e
            then w
            else emptyWidget

drawHeaderCursor :: Select -> HeaderCursor -> Widget ResourceName
drawHeaderCursor = drawTextCursor

drawHeader :: Header -> Widget ResourceName
drawHeader = txt . headerText

drawCurrentStateFromCursor :: StateHistoryCursor -> Widget ResourceName
drawCurrentStateFromCursor =
    drawCurrentState . StateHistory . NE.toList . rebuildStateHistoryCursor

drawCurrentState :: StateHistory -> Widget ResourceName
drawCurrentState (StateHistory ls)
    | null ls = emptyWidget
    | otherwise =
        withAttr todoStateAttr $
        case reverse ls of
            (she:_) ->
                case stateHistoryEntryNewState she of
                    Nothing -> emptyWidget
                    Just ts -> drawTodoState ts <+> str " "
            _ -> emptyWidget

drawContentsCursor :: Select -> ContentsCursor -> Widget ResourceName
drawContentsCursor = drawTextFieldCursor

drawContents :: Contents -> Widget ResourceName
drawContents = txt . contentsText

drawTimestampsCursor :: Select -> TimestampsCursor -> Widget ResourceName
drawTimestampsCursor _ = strWrap . show

drawTimestamps :: Map TimestampName Timestamp -> Widget ResourceName
drawTimestamps m
    | M.null m = emptyWidget
    | otherwise = strWrap $ show m

drawPropertiesCursor :: Select -> PropertiesCursor -> Widget ResourceName
drawPropertiesCursor _ = strWrap . show

drawProperties :: Map PropertyName PropertyValue -> Widget ResourceName
drawProperties m
    | M.null m = emptyWidget
    | otherwise = strWrap $ show m

drawStateHistoryCursor :: Select -> StateHistoryCursor -> Widget ResourceName
drawStateHistoryCursor _ =
    drawStateHistory . StateHistory . NE.toList . rebuildStateHistoryCursor

drawStateHistory :: StateHistory -> Widget ResourceName
drawStateHistory (StateHistory ls)
    | null ls = emptyWidget
    | otherwise =
        withAttr todoStateHistoryAttr $
        vBox $
        flip map ls $ \StateHistoryEntry {..} ->
            hBox
                [ strWrap $ show stateHistoryEntryTimestamp
                , maybe
                      emptyWidget
                      ((str " " <+>) . drawTodoState)
                      stateHistoryEntryNewState
                ]

drawTagsCursor :: Select -> TagsCursor -> Widget ResourceName
drawTagsCursor _ = strWrap . show

drawTags :: [Tag] -> Widget ResourceName
drawTags ts
    | null ts = emptyWidget
    | otherwise = strWrap $ show ts

drawLogbookCursor :: Select -> LogbookCursor -> Widget ResourceName
drawLogbookCursor _ lbc =
    case lbc of
        LogbookCursorClosed Nothing -> emptyWidget
        _ -> strWrap $ show lbc

drawLogbook :: Logbook -> Widget ResourceName
drawLogbook (LogClosed ls)
    | null ls = emptyWidget
    | otherwise = strWrap $ show ls
drawLogbook lb = strWrap $ show lb

drawTextCursor :: Select -> TextCursor -> Widget ResourceName
drawTextCursor s tc =
    (case s of
         MaybeSelected ->
             showCursor textCursorName (B.Location (textCursorIndex tc, 0))
         _ -> id) $
    txt (rebuildTextCursor tc)

drawTextFieldCursor :: Select -> TextFieldCursor -> Widget ResourceName
drawTextFieldCursor _ = strWrap . show

drawTodoState :: TodoState -> Widget ResourceName
drawTodoState ts =
    withAttr (todoStateSpecificAttr ts <> todoStateAttr) . txtWrap $
    todoStateText ts
