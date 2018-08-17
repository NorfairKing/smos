{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Smos.App
    ( mkSmosApp
    ) where

import Import

import qualified Data.List.NonEmpty as NE

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
    handleWith :: KeyMappings -> Maybe (SmosM ())
    handleWith m =
        case e of
            VtyEvent vtye ->
                case vtye of
                    Vty.EvKey k mods -> findBestMatch k mods m
                    _ -> Nothing
            _ -> Nothing

findBestMatch :: Vty.Key -> [Vty.Modifier] -> KeyMappings -> Maybe (SmosM ())
findBestMatch k mods mappings =
    let ls =
            flip mapMaybe mappings $ \case
                MapVtyExactly k_ mods_ a ->
                    if k == k_ && sort mods == sort mods_
                        then Just (High, actionFunc a ())
                        else Nothing
                MapAnyTypeableChar a ->
                    case k of
                        Vty.KChar c -> Just (Low, actionFunc a c)
                        _ -> Nothing
     in (snd . NE.head) <$> NE.nonEmpty (sortOn fst ls)

data Priority
    = High
    | Low
    deriving (Eq, Ord)

smosStartEvent :: s -> EventM n s
smosStartEvent = pure
