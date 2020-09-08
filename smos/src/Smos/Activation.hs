{-# LANGUAGE RecordWildCards #-}

module Smos.Activation
  ( currentKeyMappings,
    findActivations,
    Activation (..),
  )
where

import Cursor.DirForest
import Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Graphics.Vty as Vty
import Import
import Lens.Micro
import Smos.Cursor.Entry
import Smos.Cursor.FileBrowser
import Smos.Cursor.Report.Next
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
                  Just sfec@SmosFileEditorCursor {..} ->
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
                        nextactionReportAnys = map ((,) AnyMatcher) nextActionReportAnyMatchers
                     in (++ nextactionReportAnys) $ map ((,) SpecificMatcher) $
                          case nextActionReportCursorSelection of
                            NextActionReportSelected -> nextActionReportMatchers
                            NextActionReportFilterSelected -> nextActionReportSearchMatchers
                  ReportWaiting _ ->
                    let WaitingReportKeyMap {..} = reportsKeymapWaitingReportKeyMap
                        waitingReportAnys = map ((,) AnyMatcher) waitingReportAnyMatchers
                     in (++ waitingReportAnys) $ map ((,) SpecificMatcher) waitingReportMatchers
        BrowserSelected ->
          case editorCursorBrowserCursor of
            Nothing -> []
            Just fbc ->
              let BrowserKeyMap {..} = keyMapBrowserKeyMap
                  browserAnys = map ((,) AnyMatcher) browserKeyMapAnyMatchers
               in (++ browserAnys) $ map ((,) SpecificMatcher) $
                    case fileBrowserSelected fbc of
                      Nothing -> browserKeyMapEmptyMatchers
                      Just (_, _, InProgress _) -> browserKeyMapInProgressMatchers
                      Just (_, _, Existent _) -> browserKeyMapExistentMatchers

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

data Activation
  = Activation
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
