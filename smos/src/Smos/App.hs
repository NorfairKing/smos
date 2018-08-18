{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Smos.App
    ( mkSmosApp
    ) where

import Import

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence as Seq

import Brick.Main as B
import Brick.Types as B

import Lens.Micro

import qualified Graphics.Vty as Vty

import Smos.Cursor.Entry
import Smos.Cursor.SmosFile

import Smos.Activation
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
                             findActivations
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

smosStartEvent :: s -> EventM n s
smosStartEvent = pure
