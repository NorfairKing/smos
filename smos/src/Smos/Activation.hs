{-# LANGUAGE RecordWildCards #-}

module Smos.Activation
  ( currentKeyMappings,
    findActivations,
    Activation (..),
  )
where

import Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Graphics.Vty as Vty
import Import
import Lens.Micro
import Smos.Cursor.Entry
import Smos.Cursor.Report.Next
import Smos.Cursor.SmosFile
import Smos.History
import Smos.Keys
import Smos.Types

currentKeyMappings :: KeyMap -> EditorCursor -> [(Precedence, KeyMapping)]
currentKeyMappings KeyMap {..} EditorCursor {..} =
  case editorCursorSelection of
    HelpSelected ->
      case editorCursorHelpCursor of
        Nothing -> []
        Just HelpCursor {..} ->
          map ((,) SpecificMatcher) $
            case helpCursorSelection of
              HelpCursorHelpSelected -> helpKeyMapHelpMatchers keyMapHelpKeyMap
              HelpCursorSearchSelected -> helpKeyMapSearchMatchers keyMapHelpKeyMap
    BrowserSelected -> map ((,) SpecificMatcher) keyMapBrowserKeyMap ++ map ((,) AnyMatcher) keyMapAnyKeyMap
    FileSelected ->
      let FileKeyMap {..} = keyMapFileKeyMap
          with :: KeyMappings -> [(Precedence, KeyMapping)]
          with specificMappings =
            map ((,) SpecificMatcher) specificMappings ++ map ((,) AnyMatcher) fileKeyMapAnyMatchers ++ map ((,) AnyMatcher) keyMapAnyKeyMap
       in case historyPresent editorCursorFileCursor of
            Nothing -> with fileKeyMapEmptyMatchers
            Just sfc ->
              case sfc ^. smosFileCursorEntrySelectionL of
                WholeEntrySelected -> with fileKeyMapEntryMatchers
                HeaderSelected -> with fileKeyMapHeaderMatchers
                ContentsSelected -> with fileKeyMapContentsMatchers
                TimestampsSelected -> with fileKeyMapTimestampsMatchers
                PropertiesSelected -> with fileKeyMapPropertiesMatchers
                StateHistorySelected -> with fileKeyMapStateHistoryMatchers
                TagsSelected -> with fileKeyMapTagsMatchers
                LogbookSelected -> with fileKeyMapLogbookMatchers
    ReportSelected ->
      let ReportsKeyMap {..} = keyMapReportsKeyMap
          anys = map ((,) AnyMatcher) keyMapAnyKeyMap
       in
        case editorCursorReportCursor of
          Nothing -> anys
          Just (ReportNextActions NextActionReportCursor {..}) ->
            (++) anys
            $ map ((,) SpecificMatcher)
              $ case nextActionReportCursorSelection of
                  NextActionReportSelected -> reportsKeymapNextActionReportMatchers
                  NextActionReportFilterSelected -> reportsKeymapNextActionReportFilterMatchers

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
