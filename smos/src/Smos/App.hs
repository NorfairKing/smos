{-# LANGUAGE RecordWildCards #-}

module Smos.App
  ( mkSmosApp,
    buildInitialState,
    initStateWithCursor,
  )
where

import Brick.Main as B
import Brick.Types as B
import qualified Control.Monad.Trans.Resource as Resource (InternalState)
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence as Seq
import Data.Time
import qualified Graphics.Vty as Vty
import Import
import Smos.Actions.File
import Smos.Activation
import Smos.Draw
import Smos.Keys
import Smos.Style
import Smos.Types
import System.Exit
import UnliftIO.Resource

mkSmosApp :: Resource.InternalState -> Path Abs Dir -> SmosConfig -> App SmosState SmosEvent ResourceName
mkSmosApp res workflowDir sc =
  App
    { appDraw = smosDraw workflowDir sc,
      appChooseCursor = smosChooseCursor,
      appHandleEvent = smosHandleEvent res sc,
      appStartEvent = smosStartEvent,
      appAttrMap = defaultAttrMap
    }

smosChooseCursor :: s -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
smosChooseCursor _ = showCursorNamed ResourceTextCursor

smosHandleEvent :: Resource.InternalState -> SmosConfig -> Event -> EventM ResourceName SmosState ()
smosHandleEvent res cf e = do
  s <- get :: EventM ResourceName SmosState SmosState
  let func =
        case keyMapFunc s e (configKeyMap cf) of
          NothingActivated ->
            case e of
              B.VtyEvent (Vty.EvKey ek mods) ->
                let kp = KeyPress ek mods
                 in recordKeyPress kp
              _ -> pure ()
          KeyActivated func_ -> do
            modify (\s_ -> s_ {smosStateErrorMessages = []}) -- Clear error messages only on an activated keypress
            func_
            clearKeyHistory
          EventActivated func_ -> func_
  (mkHalt, errs) <- runSmosM res cf func
  case mkHalt of
    Stop -> B.halt
    Continue () ->
      modify (\s_ -> s_ {smosStateErrorMessages = smosStateErrorMessages s_ ++ errs})
  where
    recordKeyPress :: KeyPress -> SmosM ()
    recordKeyPress kp = modify $ \ss -> ss {smosStateKeyHistory = smosStateKeyHistory ss |> kp}
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
                      ( \ss ->
                          let dbi = smosStateDebugInfo ss
                              dbi' = dbi {debugInfoLastMatches = Just $ NE.map activationDebug nems}
                           in ss {smosStateDebugInfo = dbi'}
                      )
                    activationFunc a
            _ -> NothingActivated
        AppEvent se ->
          case se of
            SmosUpdateTime ->
              EventActivated $ do
                now <- liftIO getZonedTime
                modify
                  ( \s_ ->
                      s_
                        { smosStateTime = now,
                          smosStateCursor = editorCursorUpdateTime now $ smosStateCursor s_
                        }
                  )
            SmosSaveFile -> EventActivated saveCurrentSmosFile
        _ -> NothingActivated

data EventResult
  = KeyActivated (SmosM ())
  | EventActivated (SmosM ())
  | NothingActivated

activationDebug :: Activation -> ActivationDebug
activationDebug Activation {..} =
  ActivationDebug
    { activationDebugPrecedence = activationPrecedence,
      activationDebugPriority = activationPriority,
      activationDebugMatch = activationMatch,
      activationDebugName = activationName
    }

smosStartEvent :: EventM n s ()
smosStartEvent = pure ()

buildInitialState :: StartingPath -> ResourceT IO SmosState
buildInitialState p = do
  mErrOrEC <- startEditorCursor p
  case mErrOrEC of
    Nothing -> liftIO $ die "Failed to lock. Has this file already been opened in another instance of smos?"
    Just errOrEC -> case errOrEC of
      Left err -> liftIO $ die err
      Right ec -> do
        zt <- liftIO getZonedTime
        pure $ initStateWithCursor zt ec

initStateWithCursor :: ZonedTime -> EditorCursor -> SmosState
initStateWithCursor zt ec =
  SmosState
    { smosStateTime = zt,
      smosStateCursor = ec,
      smosStateKeyHistory = Empty,
      smosStateAsyncs = [],
      smosStateErrorMessages = [],
      smosStateDebugInfo =
        DebugInfo
          { debugInfoLastMatches = Nothing
          }
    }
