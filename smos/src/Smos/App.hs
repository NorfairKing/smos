{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Smos.App
  ( mkSmosApp
  , initState
  , initStateWithCursor
  ) where

import Import

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence as Seq
import Data.Time
import System.FileLock

import Brick.Main as B
import Brick.Types as B

import qualified Graphics.Vty as Vty

import Smos.Data

import Smos.Actions.File

import Smos.Activation
import Smos.Draw
import Smos.Keys
import Smos.Style
import Smos.Types

mkSmosApp :: SmosConfig -> App SmosState SmosEvent ResourceName
mkSmosApp sc@SmosConfig {..} =
  App
    { appDraw = smosDraw sc
    , appChooseCursor = smosChooseCursor
    , appHandleEvent = smosHandleEvent sc
    , appStartEvent = smosStartEvent
    , appAttrMap = defaultAttrMap
    }

smosChooseCursor :: s -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
smosChooseCursor _ = showCursorNamed ResourceTextCursor

smosHandleEvent :: SmosConfig -> SmosState -> Event -> EventM ResourceName (Next SmosState)
smosHandleEvent cf s e = do
  let func =
        case keyMapFunc s e (configKeyMap cf) of
          NothingActivated ->
            case e of
              B.VtyEvent (Vty.EvKey ek mods) ->
                let kp = KeyPress ek mods
                 in recordKeyPress kp
              _ -> pure ()
          KeyActivated func_ -> do
            recordCursorHistory
            func_
            clearKeyHistory
          EventActivated func_ -> func_
  (mkHalt, s') <- runSmosM cf s func
  case mkHalt of
    Stop -> B.halt s'
    Continue () -> B.continue s'
  where
    recordKeyPress :: KeyPress -> SmosM ()
    recordKeyPress kp = modify $ \ss -> ss {smosStateKeyHistory = smosStateKeyHistory ss |> kp}
    recordCursorHistory :: SmosM ()
    recordCursorHistory =
      modify $ \ss -> ss {smosStateCursorHistory = smosStateCursor ss : smosStateCursorHistory ss}
    clearKeyHistory :: SmosM ()
    clearKeyHistory = modify $ \ss -> ss {smosStateKeyHistory = Seq.empty}

keyMapFunc :: SmosState -> Event -> KeyMap -> EventResult
keyMapFunc s e km = handleRaw $ currentKeyMappings km $ smosStateCursor s
  where
    handleRaw :: [(Precedence, KeyMapping)] -> EventResult
    handleRaw m =
      case e of
        VtyEvent vtye ->
          case vtye of
            Vty.EvKey k mods ->
              case NE.nonEmpty $ findActivations (smosStateKeyHistory s) (KeyPress k mods) m of
                Nothing -> NothingActivated
                Just nems@(a :| _) ->
                  KeyActivated $ do
                    modify
                      (\ss ->
                         let dbi = smosStateDebugInfo ss
                             dbi' = dbi {debugInfoLastMatches = Just $ NE.map activationDebug nems}
                          in ss {smosStateDebugInfo = dbi'})
                    activationFunc a
            _ -> NothingActivated
        AppEvent se ->
          case se of
            SmosUpdateTime ->
              EventActivated $ do
                now <- liftIO getZonedTime
                modify
                  (\s_ ->
                     s_
                       { smosStateTime = now
                       , smosStateCursor = editorCursorUpdateTime now $ smosStateCursor s_
                       })
            SmosSaveFile -> EventActivated saveCurrentSmosFile
        _ -> NothingActivated

data EventResult
  = KeyActivated (SmosM ())
  | EventActivated (SmosM ())
  | NothingActivated

activationDebug :: Activation -> ActivationDebug
activationDebug Activation {..} =
  ActivationDebug
    { activationDebugPrecedence = activationPrecedence
    , activationDebugPriority = activationPriority
    , activationDebugMatch = activationMatch
    , activationDebugName = activationName
    }

smosStartEvent :: s -> EventM n s
smosStartEvent = pure

initState :: ZonedTime -> Path Abs File -> FileLock -> Maybe SmosFile -> SmosState
initState zt p fl msf =
  SmosState
    { smosStateTime = zt
    , smosStateStartSmosFile = msf
    , smosStateFilePath = p
    , smosStateFileLock = fl
    , smosStateCursor = makeEditorCursor $ fromMaybe emptySmosFile msf
    , smosStateKeyHistory = Empty
    , smosStateCursorHistory = []
    , smosStateAsyncs = []
    , smosStateDebugInfo = DebugInfo {debugInfoLastMatches = Nothing}
    }

initStateWithCursor :: ZonedTime -> Path Abs File -> FileLock -> EditorCursor -> SmosState
initStateWithCursor zt p fl ec =
  SmosState
    { smosStateTime = zt
    , smosStateStartSmosFile = Just $ rebuildEditorCursor ec
    , smosStateFilePath = p
    , smosStateFileLock = fl
    , smosStateCursor = ec
    , smosStateKeyHistory = Empty
    , smosStateCursorHistory = []
    , smosStateAsyncs = []
    , smosStateDebugInfo = DebugInfo {debugInfoLastMatches = Nothing}
    }
