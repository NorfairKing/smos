{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Smos.Activation
  ( currentKeyMappings,
    findActivations,
    Activation (..),
  )
where

import Cursor.DirForest
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Ord as Ord
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Graphics.Vty as Vty
import Lens.Micro
import Smos.Cursor.Entry
import Smos.Cursor.FileBrowser
import Smos.Cursor.Report.Entry
import Smos.Cursor.Report.Next
import Smos.Cursor.Report.Ongoing
import Smos.Cursor.Report.Timestamps
import Smos.Cursor.Report.Waiting
import Smos.Cursor.Report.Work
import Smos.Cursor.SmosFile
import Smos.Cursor.SmosFileEditor
import Smos.Keys
import Smos.Types

currentKeyMappings :: KeyMap -> EditorCursor -> [(Precedence, KeyMapping)]
currentKeyMappings KeyMap {..} EditorCursor {..} =
  let anys = map ((,) AnyMatcher) keyMapAnyKeyMap
   in -- Note that the ordering matters so we need to put the anys at the end.
      (++ anys) $ case editorCursorSelection of
        HelpSelected ->
          let HelpKeyMap {..} = keyMapHelpKeyMap
              helpAnys = map ((,) AnyMatcher) helpKeyMapAnyMatchers
           in (++ helpAnys) $ case editorCursorHelpCursor of
                Nothing -> []
                Just HelpCursor {..} ->
                  map ((,) SpecificMatcher) $
                    case helpCursorSelection of
                      HelpCursorHelpSelected -> helpKeyMapHelpMatchers
                      HelpCursorSearchSelected -> helpKeyMapSearchMatchers
        FileSelected ->
          let FileKeyMap {..} = keyMapFileKeyMap
              fileAnys = map ((,) AnyMatcher) fileKeyMapAnyMatchers
           in (++ fileAnys) $
                case editorCursorFileCursor of
                  Nothing -> []
                  Just sfec ->
                    map ((,) SpecificMatcher) $ case smosFileEditorCursorPresent sfec of
                      Nothing -> fileKeyMapEmptyMatchers
                      Just sfc ->
                        case sfc ^. smosFileCursorEntrySelectionL of
                          WholeEntrySelected -> fileKeyMapEntryMatchers
                          HeaderSelected -> fileKeyMapHeaderMatchers
                          ContentsSelected -> fileKeyMapContentsMatchers
                          TimestampsSelected -> fileKeyMapTimestampsMatchers
                          PropertiesSelected -> fileKeyMapPropertiesMatchers
                          StateHistorySelected -> fileKeyMapStateHistoryMatchers
                          TagsSelected -> fileKeyMapTagsMatchers
                          LogbookSelected -> fileKeyMapLogbookMatchers
        ReportSelected -> case editorCursorReportCursor of
          Nothing -> []
          Just rc ->
            let ReportsKeyMap {..} = keyMapReportsKeyMap
                reportsAnys = map ((,) AnyMatcher) reportsKeymapAnyMatchers
             in (++ reportsAnys) $ case rc of
                  ReportNextActions NextActionReportCursor {..} ->
                    let NextActionReportKeyMap {..} = reportsKeymapNextActionReportKeyMap
                        NextActionReportKeyMap _ _ _ = reportsKeymapNextActionReportKeyMap
                        nextActionReportAnys = map ((,) AnyMatcher) nextActionReportAnyMatchers
                     in (++ nextActionReportAnys) $
                          map ((,) SpecificMatcher) $
                            case entryReportCursorSelection nextActionReportCursorEntryReportCursor of
                              EntryReportSelected -> nextActionReportMatchers
                              EntryReportFilterSelected -> nextActionReportSearchMatchers
                  ReportWaiting WaitingReportCursor {..} ->
                    let WaitingReportKeyMap {..} = reportsKeymapWaitingReportKeyMap
                        WaitingReportKeyMap _ _ _ = reportsKeymapWaitingReportKeyMap
                        waitingReportAnys = map ((,) AnyMatcher) waitingReportAnyMatchers
                     in (++ waitingReportAnys) $
                          map ((,) SpecificMatcher) $
                            case entryReportCursorSelection waitingReportCursorEntryReportCursor of
                              EntryReportSelected -> waitingReportMatchers
                              EntryReportFilterSelected -> waitingReportSearchMatchers
                  ReportOngoing OngoingReportCursor {..} ->
                    let OngoingReportKeyMap {..} = reportsKeymapOngoingReportKeyMap
                        OngoingReportKeyMap _ _ _ = reportsKeymapOngoingReportKeyMap
                        ongoingReportAnys = map ((,) AnyMatcher) ongoingReportAnyMatchers
                     in (++ ongoingReportAnys) $
                          map ((,) SpecificMatcher) $
                            case entryReportCursorSelection ongoingReportCursorEntryReportCursor of
                              EntryReportSelected -> ongoingReportMatchers
                              EntryReportFilterSelected -> ongoingReportSearchMatchers
                  ReportTimestamps TimestampsReportCursor {..} ->
                    let TimestampsReportKeyMap {..} = reportsKeymapTimestampsReportKeyMap
                        TimestampsReportKeyMap _ _ _ = reportsKeymapTimestampsReportKeyMap
                        timestampsReportAnys = map ((,) AnyMatcher) timestampsReportAnyMatchers
                     in (++ timestampsReportAnys) $
                          map ((,) SpecificMatcher) $
                            case entryReportCursorSelection timestampsReportCursorEntryReportCursor of
                              EntryReportSelected -> timestampsReportMatchers
                              EntryReportFilterSelected -> timestampsReportSearchMatchers
                  ReportStuck _ ->
                    let StuckReportKeyMap {..} = reportsKeymapStuckReportKeyMap
                        StuckReportKeyMap _ _ = reportsKeymapStuckReportKeyMap
                        stuckReportAnys = map ((,) AnyMatcher) stuckReportAnyMatchers
                     in (++ stuckReportAnys) $
                          map ((,) SpecificMatcher) stuckReportMatchers
                  ReportWork WorkReportCursor {..} ->
                    let WorkReportKeyMap {..} = reportsKeymapWorkReportKeyMap
                        WorkReportKeyMap _ _ _ = reportsKeymapWorkReportKeyMap
                        workReportAnys = map ((,) AnyMatcher) workReportAnyMatchers
                     in (++ workReportAnys) $
                          map ((,) SpecificMatcher) $
                            case entryReportCursorSelection workReportCursorResultEntries of
                              EntryReportSelected -> workReportMatchers
                              EntryReportFilterSelected -> workReportSearchMatchers
        BrowserSelected ->
          case editorCursorBrowserCursor of
            Nothing -> []
            Just fbc ->
              let BrowserKeyMap {..} = keyMapBrowserKeyMap
                  BrowserKeyMap _ _ _ _ _ = keyMapBrowserKeyMap
                  browserAnys = map ((,) AnyMatcher) browserKeyMapAnyMatchers
               in (++ browserAnys) $
                    map ((,) SpecificMatcher) $
                      case fileBrowserCursorSelection fbc of
                        FileBrowserSelected ->
                          case fileBrowserSelected fbc of
                            Nothing -> browserKeyMapEmptyMatchers
                            Just (_, _, InProgress _) -> browserKeyMapInProgressMatchers
                            Just (_, _, Existent _) -> browserKeyMapExistentMatchers
                        FileBrowserFilterSelected -> browserKeyMapFilterMatchers

findActivations :: Seq KeyPress -> KeyPress -> [(Precedence, KeyMapping)] -> [Activation]
findActivations history kp mappings =
  sortActivations $ concatMap (`findExactActivations` mappings) $ tails $ toList $ history |> kp

findExactActivations :: [KeyPress] -> [(Precedence, KeyMapping)] -> [Activation]
findExactActivations history mappings =
  flip mapMaybe mappings $ \(p, m) ->
    case m of
      MapVtyExactly kp_ a ->
        case history of
          [kp] ->
            if keyPressMatch kp kp_
              then
                Just
                  Activation
                    { activationPrecedence = p,
                      activationPriority = MatchExact,
                      activationMatch = Seq.singleton kp,
                      activationName = actionName a,
                      activationFunc = actionFunc a
                    }
              else Nothing
          _ -> Nothing
      MapAnyTypeableChar a ->
        case history of
          [kp@(KeyPress k [])] ->
            case k of
              Vty.KChar c ->
                Just
                  Activation
                    { activationPrecedence = p,
                      activationPriority = MatchAnyChar,
                      activationMatch = Seq.singleton kp,
                      activationName = actionUsingName a,
                      activationFunc = actionUsingFunc a c
                    }
              _ -> Nothing
          _ -> Nothing
      MapCatchAll a ->
        case history of
          [] -> Nothing
          (kp : _) ->
            Just
              Activation
                { activationPrecedence = p,
                  activationPriority = CatchAll,
                  activationMatch = Seq.singleton kp,
                  activationName = actionName a,
                  activationFunc = actionFunc a
                }
      mc@(MapCombination _ _) ->
        let go ::
              [KeyPress] -> -- History
              KeyMapping ->
              Seq KeyPress -> -- Match
              Maybe Activation
            go hs km acc =
              case (hs, km) of
                ([], _) -> Nothing
                (hkp : _, MapCatchAll a) ->
                  Just
                    Activation
                      { activationPrecedence = p,
                        activationPriority = CatchAll,
                        activationMatch = acc |> hkp,
                        activationName = actionName a,
                        activationFunc = actionFunc a
                      }
                ([hkp], MapVtyExactly kp_ a) ->
                  if keyPressMatch hkp kp_
                    then
                      Just
                        Activation
                          { activationPrecedence = p,
                            activationPriority = MatchExact,
                            activationMatch = acc |> hkp,
                            activationName = actionName a,
                            activationFunc = actionFunc a
                          }
                    else Nothing
                ([hkp@(KeyPress hk _)], MapAnyTypeableChar a) ->
                  case hk of
                    Vty.KChar c ->
                      Just
                        Activation
                          { activationPrecedence = p,
                            activationPriority = MatchAnyChar,
                            activationMatch = acc |> hkp,
                            activationName = actionUsingName a,
                            activationFunc = actionUsingFunc a c
                          }
                    _ -> Nothing
                (hkp : hkps, MapCombination kp_ km_) ->
                  if keyPressMatch hkp kp_
                    then go hkps km_ (acc |> hkp)
                    else Nothing
                (_, _) -> Nothing
         in go history mc Seq.empty

keyPressMatch :: KeyPress -> KeyPress -> Bool
keyPressMatch (KeyPress k1 mods1) (KeyPress k2 mods2) = k1 == k2 && sort mods1 == sort mods2

data Activation = Activation
  { activationPrecedence :: Precedence,
    activationPriority :: Priority,
    activationMatch :: Seq KeyPress,
    activationName :: ActionName,
    activationFunc :: SmosM ()
  }

sortActivations :: [Activation] -> [Activation]
sortActivations =
  sortOn
    ( \Activation {..} ->
        ( Ord.Down $ length activationMatch,
          Ord.Down activationPrecedence,
          Ord.Down activationPriority
        )
    )
