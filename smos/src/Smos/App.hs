{-# LANGUAGE RecordWildCards #-}

module Smos.App
    ( mkSmosApp
    ) where

import Import

import qualified Data.Map as M

import Brick.Main as B
import Brick.Types as B

import Lens.Micro

import qualified Graphics.Vty as V

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
                        B.VtyEvent (V.EvKey ek mods) ->
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
        modify $ \ss -> ss {smosStateKeyHistory = kp : smosStateKeyHistory ss}
    clearKeyHistory :: SmosM ()
    clearKeyHistory = modify $ \ss -> ss {smosStateKeyHistory = []}

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
    handleWith :: Map KeyMatch Action -> Maybe (SmosM ())
    handleWith m =
        case e of
            VtyEvent vtye ->
                case vtye of
                    V.EvKey k mods ->
                        case M.lookup (MatchExactly k mods) m of
                            Nothing -> Nothing
                            Just a -> Just $ actionFunc a
                    _ -> Nothing
            _ -> Nothing

smosStartEvent :: s -> EventM n s
smosStartEvent = pure
