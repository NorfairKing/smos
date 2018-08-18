{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Smos.App
    ( mkSmosApp
    ) where

import Import

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Ord as Ord
import qualified Data.Sequence as Seq

import Brick.Main as B
import Brick.Types as B

import Lens.Micro

import qualified Graphics.Vty as Vty

import Smos.Cursor.Entry
import Smos.Cursor.SmosFile

import Smos.Draw
import Smos.Style
import Smos.Types

mkSmosApp :: SmosConfig -> App SmosState () ResourceName
mkSmosApp sc@SmosConfig {..} =
    App
        { appDraw = smosDraw sc
        , appChooseCursor = smosChooseCursor
        , appHandleEvent = smosHandleEvent sc
        , appStartEvent = smosStartEvent
        , appAttrMap = defaultAttrMap
        }

smosChooseCursor ::
       s -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
smosChooseCursor _ = showCursorNamed textCursorName

smosHandleEvent ::
       SmosConfig
    -> SmosState
    -> BrickEvent ResourceName ()
    -> EventM ResourceName (Next SmosState)
smosHandleEvent cf s e = do
    let func =
            case keyMapFunc s e $ configKeyMap cf of
                Nothing ->
                    case e of
                        B.VtyEvent (Vty.EvKey ek mods) ->
                            let kp = KeyPress ek mods
                             in recordKeyPress kp
                        _ -> pure ()
                Just func_ -> do
                    func_
                    clearKeyHistory
    (mkHalt, s') <- runSmosM cf s func
    case mkHalt of
        Stop -> B.halt s'
        Continue () -> B.continue s'
  where
    recordKeyPress :: KeyPress -> SmosM ()
    recordKeyPress kp =
        modify $ \ss -> ss {smosStateKeyHistory = smosStateKeyHistory ss |> kp}
    clearKeyHistory :: SmosM ()
    clearKeyHistory = modify $ \ss -> ss {smosStateKeyHistory = Seq.empty}

keyMapFunc :: SmosState -> SmosEvent -> KeyMap -> Maybe (SmosM ())
keyMapFunc s e KeyMap {..} =
    if smosStateShowHelp s
        then Just $ modify (\ss -> ss {smosStateShowHelp = False})
        else case smosStateCursor s of
                 Nothing -> handleWith keyMapEmptyMatchers
                 Just sfc ->
                     case sfc ^. smosFileCursorEntrySelectionL of
                         WholeEntrySelected -> handleWith keyMapEntryMatchers
                         HeaderSelected -> handleWith keyMapHeaderMatchers
                         ContentsSelected -> handleWith keyMapContentsMatchers
                         TimestampsSelected ->
                             handleWith keyMapTimestampsMatchers
                         PropertiesSelected ->
                             handleWith keyMapPropertiesMatchers
                         StateHistorySelected ->
                             handleWith keyMapStateHistoryMatchers
                         TagsSelected -> handleWith keyMapTagsMatchers
                         LogbookSelected -> handleWith keyMapLogbookMatchers
  where
    handleWith :: KeyMappings -> Maybe (SmosM ())
    handleWith m =
        case e of
            VtyEvent vtye ->
                case vtye of
                    Vty.EvKey k mods ->
                        case NE.nonEmpty $
                             findBestMatch
                                 (smosStateKeyHistory s)
                                 (KeyPress k mods)
                                 m of
                            Nothing -> Nothing
                            Just nems@((_, _, _, func) :| _) ->
                                Just $ do
                                    modify
                                        (\ss ->
                                             let dbi = smosStateDebugInfo ss
                                                 dbi' =
                                                     dbi
                                                         { debugInfoLastMatches =
                                                               Just $
                                                               NE.map
                                                                   (\(a, b, c, _) ->
                                                                        ( a
                                                                        , b
                                                                        , c))
                                                                   nems
                                                         }
                                              in ss {smosStateDebugInfo = dbi'})
                                    func
                    _ -> Nothing
            _ -> Nothing

findBestMatch ::
       Seq KeyPress
    -> KeyPress
    -> KeyMappings
    -> [(Priority, Seq KeyPress, Text, SmosM ())]
findBestMatch history kp@(KeyPress k _) mappings
    -- Remember, the history is in reverse, so newest presses first
 =
    let toMatch = kp <| history
        ls =
            flip mapMaybe mappings $ \case
                MapVtyExactly kp_ a ->
                    if keyPressMatch kp kp_
                        then Just
                                 ( MatchExact
                                 , Seq.singleton kp
                                 , actionName a
                                 , actionFunc a ())
                        else Nothing
                MapAnyTypeableChar a ->
                    case k of
                        Vty.KChar c ->
                            Just
                                ( MatchAnyChar
                                , Seq.singleton kp
                                , actionName a
                                , actionFunc a c)
                        _ -> Nothing
                mc@(MapCombination _ _) ->
                    let go :: Seq KeyPress -- History
                           -> KeyMapping
                           -> Seq KeyPress -- Match
                           -> Maybe (Priority, Seq KeyPress, Text, SmosM ())
                        go hs km acc =
                            case (hs, km) of
                                (Empty, _) -> Nothing
                                (hkp :<| _, MapVtyExactly kp_ a) ->
                                    if keyPressMatch hkp kp_
                                        then Just
                                                 ( MatchExact
                                                 , acc |> hkp
                                                 , actionName a
                                                 , actionFunc a ())
                                        else Nothing
                                (hkp@(KeyPress hk _) :<| _, MapAnyTypeableChar a) ->
                                    case hk of
                                        Vty.KChar c ->
                                            Just
                                                ( MatchAnyChar
                                                , acc |> hkp
                                                , actionName a
                                                , actionFunc a c)
                                        _ -> Nothing
                                (hkp :<| hkps, MapCombination kp_ km_) ->
                                    if keyPressMatch hkp kp_
                                        then go hkps km_ (acc |> hkp)
                                        else Nothing
                     in go toMatch mc Seq.empty
     in sortOn (\(p, match, _, _) -> Ord.Down (p, length match)) ls

keyPressMatch :: KeyPress -> KeyPress -> Bool
keyPressMatch (KeyPress k1 mods1) (KeyPress k2 mods2) =
    k1 == k2 && sort mods1 == sort mods2

smosStartEvent :: s -> EventM n s
smosStartEvent = pure
