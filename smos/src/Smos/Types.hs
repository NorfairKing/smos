{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Smos.Types
  ( module Smos.Types,
    module Smos.Monad,
  )
where

import Autodocodec
import Brick.Main as B (halt)
import Brick.Types (BrickEvent (..), EventM)
import Control.Concurrent.Async
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.Trans.Resource as Resource (InternalState)
import Control.Monad.Writer
import Cursor.FileOrDir
import Cursor.Simple.List.NonEmpty
import Cursor.Text
import Cursor.Types
import Data.Aeson
import qualified Data.Char as Char
import Data.Function
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Sequence (Seq)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro
import Path
import Smos.Cursor.Entry
import Smos.Cursor.FileBrowser
import Smos.Cursor.Report.Entry
import Smos.Cursor.Report.Next
import Smos.Cursor.Report.Stuck
import Smos.Cursor.Report.Timestamps
import Smos.Cursor.Report.Waiting
import Smos.Cursor.Report.Work
import Smos.Cursor.SmosFile
import Smos.Cursor.SmosFileEditor
import Smos.Keys
import Smos.Monad
import Smos.Report.OptParse.Types
import UnliftIO.Resource

data SmosConfig = SmosConfig
  { configKeyMap :: !KeyMap,
    configReportSettings :: !ReportSettings,
    configExplainerMode :: !Bool,
    configSandboxMode :: !Bool
  }
  deriving (Generic)

data KeyMap = KeyMap
  { keyMapFileKeyMap :: !FileKeyMap,
    keyMapBrowserKeyMap :: !BrowserKeyMap,
    keyMapReportsKeyMap :: !ReportsKeyMap,
    keyMapHelpKeyMap :: !HelpKeyMap,
    keyMapAnyKeyMap :: !KeyMappings
  }
  deriving (Generic)

instance Semigroup KeyMap where
  (<>) km1 km2 =
    KeyMap
      { keyMapFileKeyMap = keyMapFileKeyMap km1 <> keyMapFileKeyMap km2,
        keyMapBrowserKeyMap = keyMapBrowserKeyMap km1 <> keyMapBrowserKeyMap km2,
        keyMapReportsKeyMap = keyMapReportsKeyMap km1 <> keyMapReportsKeyMap km2,
        keyMapHelpKeyMap = keyMapHelpKeyMap km1 <> keyMapHelpKeyMap km2,
        keyMapAnyKeyMap = keyMapAnyKeyMap km1 <> keyMapAnyKeyMap km2
      }

instance Monoid KeyMap where
  mempty =
    KeyMap {keyMapFileKeyMap = mempty, keyMapBrowserKeyMap = mempty, keyMapReportsKeyMap = mempty, keyMapHelpKeyMap = mempty, keyMapAnyKeyMap = mempty}

keyMapActions :: KeyMap -> [AnyAction]
keyMapActions (KeyMap keyMapFileKeyMap keyMapBrowserKeyMap keyMapReportsKeyMap keyMapHelpKeyMap keyMapAnyKeyMap) =
  concat
    [ fileKeyMapActions keyMapFileKeyMap,
      browserKeyMapActions keyMapBrowserKeyMap,
      reportsKeyMapActions keyMapReportsKeyMap,
      helpKeyMapActions keyMapHelpKeyMap,
      keyMappingsActions keyMapAnyKeyMap
    ]

data FileKeyMap = FileKeyMap
  { fileKeyMapEmptyMatchers :: !KeyMappings,
    fileKeyMapEntryMatchers :: !KeyMappings,
    fileKeyMapHeaderMatchers :: !KeyMappings,
    fileKeyMapContentsMatchers :: !KeyMappings,
    fileKeyMapTimestampsMatchers :: !KeyMappings,
    fileKeyMapPropertiesMatchers :: !KeyMappings,
    fileKeyMapStateHistoryMatchers :: !KeyMappings,
    fileKeyMapTagsMatchers :: !KeyMappings,
    fileKeyMapLogbookMatchers :: !KeyMappings,
    fileKeyMapAnyMatchers :: !KeyMappings
  }
  deriving (Generic)

instance Semigroup FileKeyMap where
  (<>) km1 km2 =
    FileKeyMap
      { fileKeyMapEmptyMatchers = fileKeyMapEmptyMatchers km1 <> fileKeyMapEmptyMatchers km2,
        fileKeyMapEntryMatchers = fileKeyMapEntryMatchers km1 <> fileKeyMapEntryMatchers km2,
        fileKeyMapHeaderMatchers = fileKeyMapHeaderMatchers km1 <> fileKeyMapHeaderMatchers km2,
        fileKeyMapContentsMatchers =
          fileKeyMapContentsMatchers km1 <> fileKeyMapContentsMatchers km2,
        fileKeyMapTimestampsMatchers =
          fileKeyMapTimestampsMatchers km1 <> fileKeyMapTimestampsMatchers km2,
        fileKeyMapPropertiesMatchers =
          fileKeyMapPropertiesMatchers km1 <> fileKeyMapPropertiesMatchers km2,
        fileKeyMapStateHistoryMatchers =
          fileKeyMapStateHistoryMatchers km1 <> fileKeyMapStateHistoryMatchers km2,
        fileKeyMapTagsMatchers = fileKeyMapTagsMatchers km1 <> fileKeyMapTagsMatchers km2,
        fileKeyMapLogbookMatchers = fileKeyMapLogbookMatchers km1 <> fileKeyMapLogbookMatchers km2,
        fileKeyMapAnyMatchers = fileKeyMapAnyMatchers km1 <> fileKeyMapAnyMatchers km2
      }

instance Monoid FileKeyMap where
  mempty =
    FileKeyMap
      { fileKeyMapEmptyMatchers = mempty,
        fileKeyMapEntryMatchers = mempty,
        fileKeyMapHeaderMatchers = mempty,
        fileKeyMapContentsMatchers = mempty,
        fileKeyMapTimestampsMatchers = mempty,
        fileKeyMapPropertiesMatchers = mempty,
        fileKeyMapStateHistoryMatchers = mempty,
        fileKeyMapTagsMatchers = mempty,
        fileKeyMapLogbookMatchers = mempty,
        fileKeyMapAnyMatchers = mempty
      }

fileKeyMapActions :: FileKeyMap -> [AnyAction]
fileKeyMapActions
  ( FileKeyMap
      fileKeyMapEmptyMatchers
      fileKeyMapEntryMatchers
      fileKeyMapHeaderMatchers
      fileKeyMapContentsMatchers
      fileKeyMapTimestampsMatchers
      fileKeyMapPropertiesMatchers
      fileKeyMapStateHistoryMatchers
      fileKeyMapTagsMatchers
      fileKeyMapLogbookMatchers
      fileKeyMapAnyMatchers
    ) =
    concatMap
      keyMappingsActions
      [ fileKeyMapEmptyMatchers,
        fileKeyMapEntryMatchers,
        fileKeyMapHeaderMatchers,
        fileKeyMapContentsMatchers,
        fileKeyMapTimestampsMatchers,
        fileKeyMapPropertiesMatchers,
        fileKeyMapStateHistoryMatchers,
        fileKeyMapTagsMatchers,
        fileKeyMapLogbookMatchers,
        fileKeyMapAnyMatchers
      ]

data BrowserKeyMap = BrowserKeyMap
  { browserKeyMapExistentMatchers :: KeyMappings,
    browserKeyMapInProgressMatchers :: KeyMappings,
    browserKeyMapEmptyMatchers :: KeyMappings,
    browserKeyMapFilterMatchers :: KeyMappings,
    browserKeyMapAnyMatchers :: KeyMappings
  }
  deriving (Generic)

browserKeyMapActions :: BrowserKeyMap -> [AnyAction]
browserKeyMapActions BrowserKeyMap {..} =
  let BrowserKeyMap _ _ _ _ _ = undefined
   in concatMap
        keyMappingsActions
        [ browserKeyMapExistentMatchers,
          browserKeyMapInProgressMatchers,
          browserKeyMapEmptyMatchers,
          browserKeyMapFilterMatchers,
          browserKeyMapAnyMatchers
        ]

instance Semigroup BrowserKeyMap where
  bkm1 <> bkm2 =
    let BrowserKeyMap _ _ _ _ _ = undefined
     in BrowserKeyMap
          { browserKeyMapExistentMatchers = browserKeyMapExistentMatchers bkm1 <> browserKeyMapExistentMatchers bkm2,
            browserKeyMapInProgressMatchers = browserKeyMapInProgressMatchers bkm1 <> browserKeyMapInProgressMatchers bkm2,
            browserKeyMapEmptyMatchers = browserKeyMapEmptyMatchers bkm1 <> browserKeyMapEmptyMatchers bkm2,
            browserKeyMapFilterMatchers = browserKeyMapEmptyMatchers bkm1 <> browserKeyMapEmptyMatchers bkm2,
            browserKeyMapAnyMatchers = browserKeyMapAnyMatchers bkm1 <> browserKeyMapAnyMatchers bkm2
          }

instance Monoid BrowserKeyMap where
  mempty =
    let BrowserKeyMap _ _ _ _ _ = undefined
     in BrowserKeyMap
          { browserKeyMapExistentMatchers = mempty,
            browserKeyMapInProgressMatchers = mempty,
            browserKeyMapEmptyMatchers = mempty,
            browserKeyMapFilterMatchers = mempty,
            browserKeyMapAnyMatchers = mempty
          }

data ReportsKeyMap = ReportsKeyMap
  { reportsKeymapNextActionReportKeyMap :: !NextActionReportKeyMap,
    reportsKeymapWaitingReportKeyMap :: !WaitingReportKeyMap,
    reportsKeymapTimestampsReportKeyMap :: !TimestampsReportKeyMap,
    reportsKeymapStuckReportKeyMap :: !StuckReportKeyMap,
    reportsKeymapWorkReportKeyMap :: !WorkReportKeyMap,
    reportsKeymapAnyMatchers :: !KeyMappings
  }
  deriving (Generic)

instance Semigroup ReportsKeyMap where
  rkm1 <> rkm2 =
    let ReportsKeyMap _ _ _ _ _ _ = undefined
     in ReportsKeyMap
          { reportsKeymapNextActionReportKeyMap =
              reportsKeymapNextActionReportKeyMap rkm1 <> reportsKeymapNextActionReportKeyMap rkm2,
            reportsKeymapWaitingReportKeyMap =
              reportsKeymapWaitingReportKeyMap rkm1 <> reportsKeymapWaitingReportKeyMap rkm2,
            reportsKeymapTimestampsReportKeyMap =
              reportsKeymapTimestampsReportKeyMap rkm1 <> reportsKeymapTimestampsReportKeyMap rkm2,
            reportsKeymapStuckReportKeyMap =
              reportsKeymapStuckReportKeyMap rkm1 <> reportsKeymapStuckReportKeyMap rkm2,
            reportsKeymapWorkReportKeyMap =
              reportsKeymapWorkReportKeyMap rkm1 <> reportsKeymapWorkReportKeyMap rkm2,
            reportsKeymapAnyMatchers =
              reportsKeymapAnyMatchers rkm1 <> reportsKeymapAnyMatchers rkm2
          }

instance Monoid ReportsKeyMap where
  mempty =
    ReportsKeyMap
      { reportsKeymapNextActionReportKeyMap = mempty,
        reportsKeymapWaitingReportKeyMap = mempty,
        reportsKeymapTimestampsReportKeyMap = mempty,
        reportsKeymapStuckReportKeyMap = mempty,
        reportsKeymapWorkReportKeyMap = mempty,
        reportsKeymapAnyMatchers = mempty
      }

reportsKeyMapActions :: ReportsKeyMap -> [AnyAction]
reportsKeyMapActions ReportsKeyMap {..} =
  let ReportsKeyMap _ _ _ _ _ _ = undefined
   in concat
        [ nextActionReportKeyMapActions reportsKeymapNextActionReportKeyMap,
          waitingReportKeyMapActions reportsKeymapWaitingReportKeyMap,
          timestampsReportKeyMapActions reportsKeymapTimestampsReportKeyMap,
          stuckReportKeyMapActions reportsKeymapStuckReportKeyMap,
          workReportKeyMapActions reportsKeymapWorkReportKeyMap,
          keyMappingsActions reportsKeymapAnyMatchers
        ]

data NextActionReportKeyMap = NextActionReportKeyMap
  { nextActionReportMatchers :: KeyMappings,
    nextActionReportSearchMatchers :: KeyMappings,
    nextActionReportAnyMatchers :: KeyMappings
  }
  deriving (Generic)

instance Semigroup NextActionReportKeyMap where
  narkm1 <> narkm2 =
    let NextActionReportKeyMap _ _ _ = undefined
     in NextActionReportKeyMap
          { nextActionReportMatchers = nextActionReportMatchers narkm1 <> nextActionReportMatchers narkm2,
            nextActionReportSearchMatchers = nextActionReportSearchMatchers narkm1 <> nextActionReportSearchMatchers narkm2,
            nextActionReportAnyMatchers = nextActionReportAnyMatchers narkm1 <> nextActionReportAnyMatchers narkm2
          }

instance Monoid NextActionReportKeyMap where
  mappend = (<>)
  mempty =
    NextActionReportKeyMap
      { nextActionReportMatchers = mempty,
        nextActionReportSearchMatchers = mempty,
        nextActionReportAnyMatchers = mempty
      }

nextActionReportKeyMapActions :: NextActionReportKeyMap -> [AnyAction]
nextActionReportKeyMapActions NextActionReportKeyMap {..} =
  let NextActionReportKeyMap _ _ _ = undefined
   in concatMap
        keyMappingsActions
        [ nextActionReportMatchers,
          nextActionReportSearchMatchers,
          nextActionReportAnyMatchers
        ]

data WaitingReportKeyMap = WaitingReportKeyMap
  { waitingReportMatchers :: KeyMappings,
    waitingReportSearchMatchers :: KeyMappings,
    waitingReportAnyMatchers :: KeyMappings
  }
  deriving (Generic)

instance Semigroup WaitingReportKeyMap where
  narkm1 <> narkm2 =
    let WaitingReportKeyMap _ _ _ = undefined
     in WaitingReportKeyMap
          { waitingReportMatchers = waitingReportMatchers narkm1 <> waitingReportMatchers narkm2,
            waitingReportSearchMatchers = waitingReportSearchMatchers narkm1 <> waitingReportSearchMatchers narkm2,
            waitingReportAnyMatchers = waitingReportAnyMatchers narkm1 <> waitingReportAnyMatchers narkm2
          }

instance Monoid WaitingReportKeyMap where
  mappend = (<>)
  mempty =
    WaitingReportKeyMap
      { waitingReportMatchers = mempty,
        waitingReportSearchMatchers = mempty,
        waitingReportAnyMatchers = mempty
      }

waitingReportKeyMapActions :: WaitingReportKeyMap -> [AnyAction]
waitingReportKeyMapActions WaitingReportKeyMap {..} =
  let WaitingReportKeyMap _ _ _ = undefined
   in concatMap
        keyMappingsActions
        [ waitingReportMatchers,
          waitingReportSearchMatchers,
          waitingReportAnyMatchers
        ]

data TimestampsReportKeyMap = TimestampsReportKeyMap
  { timestampsReportMatchers :: KeyMappings,
    timestampsReportSearchMatchers :: KeyMappings,
    timestampsReportAnyMatchers :: KeyMappings
  }
  deriving (Generic)

instance Semigroup TimestampsReportKeyMap where
  narkm1 <> narkm2 =
    let TimestampsReportKeyMap _ _ _ = undefined
     in TimestampsReportKeyMap
          { timestampsReportMatchers = timestampsReportMatchers narkm1 <> timestampsReportMatchers narkm2,
            timestampsReportSearchMatchers = timestampsReportSearchMatchers narkm1 <> timestampsReportSearchMatchers narkm2,
            timestampsReportAnyMatchers = timestampsReportAnyMatchers narkm1 <> timestampsReportAnyMatchers narkm2
          }

instance Monoid TimestampsReportKeyMap where
  mappend = (<>)
  mempty =
    TimestampsReportKeyMap
      { timestampsReportMatchers = mempty,
        timestampsReportSearchMatchers = mempty,
        timestampsReportAnyMatchers = mempty
      }

timestampsReportKeyMapActions :: TimestampsReportKeyMap -> [AnyAction]
timestampsReportKeyMapActions TimestampsReportKeyMap {..} =
  let TimestampsReportKeyMap _ _ _ = undefined
   in concatMap
        keyMappingsActions
        [ timestampsReportMatchers,
          timestampsReportSearchMatchers,
          timestampsReportAnyMatchers
        ]

data StuckReportKeyMap = StuckReportKeyMap
  { stuckReportMatchers :: KeyMappings,
    stuckReportAnyMatchers :: KeyMappings
  }
  deriving (Generic)

instance Semigroup StuckReportKeyMap where
  narkm1 <> narkm2 =
    let StuckReportKeyMap _ _ = undefined
     in StuckReportKeyMap
          { stuckReportMatchers = stuckReportMatchers narkm1 <> stuckReportMatchers narkm2,
            stuckReportAnyMatchers = stuckReportAnyMatchers narkm1 <> stuckReportAnyMatchers narkm2
          }

instance Monoid StuckReportKeyMap where
  mappend = (<>)
  mempty =
    StuckReportKeyMap
      { stuckReportMatchers = mempty,
        stuckReportAnyMatchers = mempty
      }

stuckReportKeyMapActions :: StuckReportKeyMap -> [AnyAction]
stuckReportKeyMapActions StuckReportKeyMap {..} =
  let StuckReportKeyMap _ _ = undefined
   in concatMap
        keyMappingsActions
        [ stuckReportMatchers,
          stuckReportAnyMatchers
        ]

data WorkReportKeyMap = WorkReportKeyMap
  { workReportMatchers :: KeyMappings,
    workReportSearchMatchers :: KeyMappings,
    workReportAnyMatchers :: KeyMappings
  }
  deriving (Generic)

instance Semigroup WorkReportKeyMap where
  narkm1 <> narkm2 =
    let WorkReportKeyMap _ _ _ = undefined
     in WorkReportKeyMap
          { workReportMatchers = workReportMatchers narkm1 <> workReportMatchers narkm2,
            workReportSearchMatchers = workReportSearchMatchers narkm1 <> workReportSearchMatchers narkm2,
            workReportAnyMatchers = workReportAnyMatchers narkm1 <> workReportAnyMatchers narkm2
          }

instance Monoid WorkReportKeyMap where
  mappend = (<>)
  mempty =
    WorkReportKeyMap
      { workReportMatchers = mempty,
        workReportSearchMatchers = mempty,
        workReportAnyMatchers = mempty
      }

workReportKeyMapActions :: WorkReportKeyMap -> [AnyAction]
workReportKeyMapActions WorkReportKeyMap {..} =
  let WorkReportKeyMap _ _ _ = undefined
   in concatMap
        keyMappingsActions
        [ workReportMatchers,
          workReportSearchMatchers,
          workReportAnyMatchers
        ]

keyMapHelpMatchers :: KeyMap -> KeyMappings
keyMapHelpMatchers km =
  let HelpKeyMap {..} = keyMapHelpKeyMap km
   in helpKeyMapHelpMatchers <> helpKeyMapSearchMatchers

data HelpKeyMap = HelpKeyMap
  { helpKeyMapHelpMatchers :: !KeyMappings,
    helpKeyMapSearchMatchers :: !KeyMappings,
    helpKeyMapAnyMatchers :: !KeyMappings
  }
  deriving (Generic)

instance Semigroup HelpKeyMap where
  hkm1 <> hkm2 =
    let HelpKeyMap _ _ _ = undefined
     in HelpKeyMap
          { helpKeyMapHelpMatchers = helpKeyMapHelpMatchers hkm1 <> helpKeyMapHelpMatchers hkm2,
            helpKeyMapSearchMatchers = helpKeyMapSearchMatchers hkm1 <> helpKeyMapSearchMatchers hkm2,
            helpKeyMapAnyMatchers = helpKeyMapAnyMatchers hkm1 <> helpKeyMapAnyMatchers hkm2
          }

instance Monoid HelpKeyMap where
  mempty =
    let HelpKeyMap _ _ _ = undefined
     in HelpKeyMap
          { helpKeyMapHelpMatchers = mempty,
            helpKeyMapSearchMatchers = mempty,
            helpKeyMapAnyMatchers = mempty
          }

helpKeyMapActions :: HelpKeyMap -> [AnyAction]
helpKeyMapActions HelpKeyMap {..} =
  let HelpKeyMap _ _ _ = undefined
   in concatMap
        keyMappingsActions
        [ helpKeyMapHelpMatchers,
          helpKeyMapSearchMatchers,
          helpKeyMapAnyMatchers
        ]

type KeyMappings = [KeyMapping]

keyMappingsActions :: KeyMappings -> [AnyAction]
keyMappingsActions = map keyMappingAction

data KeyMapping
  = MapVtyExactly !KeyPress !Action
  | MapAnyTypeableChar !(ActionUsing Char)
  | MapCatchAll !Action
  | MapCombination !KeyPress !KeyMapping

keyMappingAction :: KeyMapping -> AnyAction
keyMappingAction = \case
  MapVtyExactly _ a -> PlainAction a
  MapAnyTypeableChar au -> UsingCharAction au
  MapCatchAll a -> PlainAction a
  MapCombination _ km -> keyMappingAction km

newtype ActionName = ActionName
  { actionNameText :: Text
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype (IsString, Semigroup, Monoid)
  deriving (FromJSON, ToJSON) via (Autodocodec ActionName)

instance Validity ActionName

instance HasCodec ActionName where
  codec = dimapCodec ActionName actionNameText codec

data Action = Action
  { actionName :: ActionName,
    actionFunc :: SmosM (),
    actionDescription :: Text
  }
  deriving (Generic)

data ActionUsing a = ActionUsing
  { actionUsingName :: ActionName,
    actionUsingFunc :: a -> SmosM (),
    actionUsingDescription :: Text
  }
  deriving (Generic)

data AnyAction
  = PlainAction Action
  | UsingCharAction (ActionUsing Char)

anyActionName :: AnyAction -> ActionName
anyActionName (PlainAction a) = actionName a
anyActionName (UsingCharAction au) = actionUsingName au

anyActionDescription :: AnyAction -> Text
anyActionDescription (PlainAction a) = actionDescription a
anyActionDescription (UsingCharAction au) = actionUsingDescription au

type Event = BrickEvent ResourceName SmosEvent

data SmosEvent
  = SmosUpdateTime
  | SmosSaveFile

type SmosM = MkSmosM ResourceName SmosConfig SmosState

runSmosM ::
  Resource.InternalState ->
  SmosConfig ->
  SmosM a ->
  EventM ResourceName SmosState (a, [Text])
runSmosM = runMkSmosM

data SmosState = SmosState
  { smosStateTime :: !ZonedTime,
    smosStateCursor :: !EditorCursor,
    smosStateKeyHistory :: !(Seq KeyPress),
    smosStateAsyncs :: ![Async ()],
    smosStateDebugInfo :: !DebugInfo,
    smosStateErrorMessages :: [Text] -- In reverse order
  }
  deriving (Generic)

addErrorMessage :: Text -> SmosM ()
addErrorMessage t = tell [t]

runSmosAsync :: IO () -> SmosM ()
runSmosAsync func = do
  (_, a) <- allocate (async func) wait
  modify (\ss -> ss {smosStateAsyncs = a : smosStateAsyncs ss})

data DebugInfo = DebugInfo
  { debugInfoLastMatches :: Maybe (NonEmpty ActivationDebug)
  }
  deriving (Show, Eq, Generic)

data ActivationDebug = ActivationDebug
  { activationDebugPrecedence :: Precedence,
    activationDebugPriority :: Priority,
    activationDebugMatch :: Seq KeyPress,
    activationDebugName :: ActionName
  }
  deriving (Show, Eq, Generic)

data Priority
  = CatchAll
  | MatchAnyChar
  | MatchExact -- Has higher priority.
  deriving (Show, Eq, Ord)

data Precedence
  = AnyMatcher
  | SpecificMatcher -- Has higher priority.
  deriving (Show, Eq, Ord)

data ResourceName
  = ResourceTextCursor
  | ResourceViewport
  deriving (Show, Eq, Ord, Generic)

quit :: Action
quit =
  Action
    { actionName = "quit",
      actionDescription = "Quit Smos",
      actionFunc = liftEventM B.halt
    }

-- [ Help Cursor ] --
-- I cannot factor this out because of the following circular dependency:
--
-- HelpCursor -> KeyMapping
--    ^             |
--    |             v
-- SmosState <- SmosM
--
-- and EditorCursor depends on HelpCursor, so that has the same problem
data HelpCursor = HelpCursor
  { helpCursorTitle :: Text,
    helpCursorSearchBar :: TextCursor,
    helpCursorSelectedKeyHelpCursors :: Maybe (NonEmptyCursor KeyHelpCursor),
    helpCursorKeyHelpCursors :: [KeyHelpCursor],
    helpCursorSelection :: HelpCursorSelection
  }
  deriving (Show, Eq, Generic)

instance Validity HelpCursor

data HelpCursorSelection
  = HelpCursorHelpSelected
  | HelpCursorSearchSelected
  deriving (Show, Eq, Generic)

instance Validity HelpCursorSelection

makeHelpCursor :: Text -> KeyMappings -> HelpCursor
makeHelpCursor title kms =
  HelpCursor
    { helpCursorTitle = title,
      helpCursorSearchBar = emptyTextCursor,
      helpCursorSelectedKeyHelpCursors = hcs,
      helpCursorKeyHelpCursors = khcs,
      helpCursorSelection = HelpCursorHelpSelected
    }
  where
    hcs = makeNonEmptyCursor <$> NE.nonEmpty khcs
    khcs = makeKeyHelpCursors kms

makeKeyHelpCursors :: KeyMappings -> [KeyHelpCursor]
makeKeyHelpCursors = sortKeyHelpCursors . combine . map makeKeyHelpCursor
  where
    sortKeyHelpCursors = sortOn $ \khc -> case keyHelpCursorKeyBinding khc of
      [] -> Nothing
      kbs -> Just $ minimum $ map keyCombinationComplexity kbs
    combine =
      map (combineKeyHelpCursors . NE.fromList) -- Safe because of 'groupBy'
        . groupBy ((==) `on` keyHelpCursorName)
        . sortBy (compare `on` keyHelpCursorName)

makeKeyHelpCursor :: KeyMapping -> KeyHelpCursor
makeKeyHelpCursor km =
  case km of
    MapVtyExactly kp a ->
      KeyHelpCursor
        { keyHelpCursorKeyBinding = [PressExactly kp],
          keyHelpCursorName = actionName a,
          keyHelpCursorDescription = actionDescription a
        }
    MapAnyTypeableChar au ->
      KeyHelpCursor
        { keyHelpCursorKeyBinding = [PressAnyChar],
          keyHelpCursorName = actionUsingName au,
          keyHelpCursorDescription = actionUsingDescription au
        }
    MapCatchAll a ->
      KeyHelpCursor
        { keyHelpCursorKeyBinding = [PressAny],
          keyHelpCursorName = actionName a,
          keyHelpCursorDescription = actionDescription a
        }
    MapCombination kp km_ ->
      let khc = makeKeyHelpCursor km_
       in khc
            { keyHelpCursorKeyBinding = map (PressCombination kp) (keyHelpCursorKeyBinding khc)
            }

helpCursorKeySearchBarL :: Lens' HelpCursor TextCursor
helpCursorKeySearchBarL =
  lens helpCursorSearchBar $ \hc tc ->
    let query = rebuildTextCursor tc
        selected = searchHelpCursor query $ helpCursorKeyHelpCursors hc
     in hc
          { helpCursorSearchBar = tc,
            helpCursorSelectedKeyHelpCursors = makeNonEmptyCursor <$> NE.nonEmpty selected
          }

searchHelpCursor :: Text -> [KeyHelpCursor] -> [KeyHelpCursor]
searchHelpCursor query =
  filter ((T.toCaseFold query `T.isInfixOf`) . T.toCaseFold . actionNameText . keyHelpCursorName)

helpCursorSelectedKeyHelpCursorsL :: Lens' HelpCursor (Maybe (NonEmptyCursor KeyHelpCursor))
helpCursorSelectedKeyHelpCursorsL =
  lens helpCursorSelectedKeyHelpCursors $ \hc ne -> hc {helpCursorSelectedKeyHelpCursors = ne}

helpCursorUp :: HelpCursor -> Maybe HelpCursor
helpCursorUp =
  helpCursorSelectedKeyHelpCursorsL $ \msc ->
    Just
      <$> case msc of
        Nothing -> Nothing
        Just sc -> nonEmptyCursorSelectPrev sc

helpCursorDown :: HelpCursor -> Maybe HelpCursor
helpCursorDown =
  helpCursorSelectedKeyHelpCursorsL $ \msc ->
    Just
      <$> case msc of
        Nothing -> Nothing
        Just sc -> nonEmptyCursorSelectNext sc

helpCursorStart :: HelpCursor -> HelpCursor
helpCursorStart = helpCursorSelectedKeyHelpCursorsL %~ fmap nonEmptyCursorSelectFirst

helpCursorEnd :: HelpCursor -> HelpCursor
helpCursorEnd = helpCursorSelectedKeyHelpCursorsL %~ fmap nonEmptyCursorSelectLast

helpCursorSelectionL :: Lens' HelpCursor HelpCursorSelection
helpCursorSelectionL = lens helpCursorSelection $ \hc hcs -> hc {helpCursorSelection = hcs}

helpCursorSelectHelp :: HelpCursor -> Maybe HelpCursor
helpCursorSelectHelp =
  helpCursorSelectionL $ \case
    HelpCursorSearchSelected -> Just HelpCursorHelpSelected
    HelpCursorHelpSelected -> Nothing

helpCursorSelectSearch :: HelpCursor -> Maybe HelpCursor
helpCursorSelectSearch =
  helpCursorSelectionL $ \case
    HelpCursorHelpSelected -> Just HelpCursorSearchSelected
    HelpCursorSearchSelected -> Nothing

helpCursorToggleSelection :: HelpCursor -> HelpCursor
helpCursorToggleSelection =
  helpCursorSelectionL
    %~ ( \case
           HelpCursorHelpSelected -> HelpCursorSearchSelected
           HelpCursorSearchSelected -> HelpCursorHelpSelected
       )

helpCursorInsert :: Char -> HelpCursor -> Maybe HelpCursor
helpCursorInsert c = helpCursorKeySearchBarL $ textCursorInsert c

helpCursorAppend :: Char -> HelpCursor -> Maybe HelpCursor
helpCursorAppend c = helpCursorKeySearchBarL $ textCursorAppend c

helpCursorRemove :: HelpCursor -> Maybe HelpCursor
helpCursorRemove =
  helpCursorKeySearchBarL $ \tc ->
    case textCursorRemove tc of
      Nothing -> Nothing
      Just Deleted -> Nothing
      Just (Updated hc) -> Just hc

helpCursorDelete :: HelpCursor -> Maybe HelpCursor
helpCursorDelete =
  helpCursorKeySearchBarL $ \tc ->
    case textCursorDelete tc of
      Nothing -> Nothing
      Just Deleted -> Nothing
      Just (Updated hc) -> Just hc

data KeyHelpCursor = KeyHelpCursor
  { keyHelpCursorKeyBinding :: [KeyCombination],
    keyHelpCursorName :: ActionName,
    keyHelpCursorDescription :: Text
  }
  deriving (Show, Eq, Generic)

instance Validity KeyHelpCursor

combineKeyHelpCursors :: NonEmpty KeyHelpCursor -> KeyHelpCursor
combineKeyHelpCursors (khc :| rest) =
  khc
    { keyHelpCursorKeyBinding =
        concat (keyHelpCursorKeyBinding khc : map keyHelpCursorKeyBinding rest)
    }

data KeyCombination
  = PressExactly KeyPress
  | PressAnyChar
  | PressAny
  | PressCombination KeyPress KeyCombination
  deriving (Show, Eq, Ord, Generic)

instance Validity KeyCombination

renderKeyCombination :: KeyCombination -> Text
renderKeyCombination = go
  where
    go :: KeyCombination -> Text
    go (PressExactly kp) = renderKeyPress kp
    go PressAnyChar = "<any char>"
    go PressAny = "<any key>"
    go (PressCombination kp km) = renderKeyPress kp <> go km

-- Some measure of the complexity of a key combination.
-- This is only for ordering them in the help cursor
keyCombinationComplexity :: KeyCombination -> Word
keyCombinationComplexity = \case
  PressExactly kp -> keyPressComplexity kp
  PressAnyChar -> 2
  PressAny -> 2
  PressCombination kp kc -> 1 + keyPressComplexity kp + keyCombinationComplexity kc

keyPressComplexity :: KeyPress -> Word
keyPressComplexity (KeyPress key mods) = (+ genericLength mods) $ case key of
  KChar c -> if Char.isAlpha c then 1 else 2
  _ -> 3

-- [ Editor Cursor ] --
--
-- Cannot factor this out because of the problem with help cursor.
data EditorCursor = EditorCursor
  { editorCursorLastOpenedFile :: Maybe (Path Abs File),
    editorCursorFileCursor :: Maybe SmosFileEditorCursor,
    editorCursorBrowserCursor :: Maybe FileBrowserCursor,
    editorCursorReportCursor :: Maybe ReportCursor,
    editorCursorHelpCursor :: Maybe HelpCursor,
    editorCursorSelection :: EditorSelection
  }

data StartingPath
  = StartingFile (Path Abs File)
  | StartingDir (Path Abs Dir)
  deriving (Show, Eq)

startEditorCursor :: MonadResource m => StartingPath -> m (Maybe (Either String EditorCursor))
startEditorCursor st = case st of
  StartingFile fp -> do
    mErrOrCursor <- startSmosFileEditorCursor fp
    let go sfec =
          EditorCursor
            { editorCursorLastOpenedFile = Just fp,
              editorCursorFileCursor = Just sfec,
              editorCursorBrowserCursor = Nothing,
              editorCursorReportCursor = Nothing,
              editorCursorHelpCursor = Nothing,
              editorCursorSelection = FileSelected
            }
    pure $ fmap (fmap go) mErrOrCursor
  StartingDir dp -> do
    fbc <- startFileBrowserCursor dp
    pure $
      Just $
        Right $
          EditorCursor
            { editorCursorLastOpenedFile = Nothing,
              editorCursorFileCursor = Nothing,
              editorCursorBrowserCursor = Just fbc,
              editorCursorReportCursor = Nothing,
              editorCursorHelpCursor = Nothing,
              editorCursorSelection = BrowserSelected
            }

editorCursorFileCursorL :: Lens' EditorCursor (Maybe SmosFileEditorCursor)
editorCursorFileCursorL =
  lens editorCursorFileCursor $ \ec msfc -> ec {editorCursorFileCursor = msfc}

editorCursorBrowserCursorL :: Lens' EditorCursor (Maybe FileBrowserCursor)
editorCursorBrowserCursorL =
  lens editorCursorBrowserCursor $ \ec msfc -> ec {editorCursorBrowserCursor = msfc}

editorCursorReportCursorL :: Lens' EditorCursor (Maybe ReportCursor)
editorCursorReportCursorL =
  lens editorCursorReportCursor $ \ec msfc -> ec {editorCursorReportCursor = msfc}

editorCursorHelpCursorL :: Lens' EditorCursor (Maybe HelpCursor)
editorCursorHelpCursorL =
  lens editorCursorHelpCursor $ \ec msfc -> ec {editorCursorHelpCursor = msfc}

editorCursorSelectionL :: Lens' EditorCursor EditorSelection
editorCursorSelectionL = lens editorCursorSelection $ \ec es -> ec {editorCursorSelection = es}

editorCursorSelect :: EditorSelection -> EditorCursor -> EditorCursor
editorCursorSelect s = editorCursorSelectionL .~ s

editorCursorSwitchToHelp :: KeyMap -> EditorCursor -> EditorCursor
editorCursorSwitchToHelp km@KeyMap {..} ec =
  let withHelpBindings n ms = Just $ makeHelpCursor n $ ms ++ keyMapHelpMatchers km ++ keyMapAnyKeyMap
   in ec
        { editorCursorHelpCursor = case editorCursorSelection ec of
            FileSelected -> case editorCursorFileCursor ec of
              Nothing -> Nothing
              Just sfec ->
                let FileKeyMap {..} = keyMapFileKeyMap
                 in ( \(t, ms) ->
                        withHelpBindings t $ ms ++ fileKeyMapAnyMatchers
                    )
                      $ case smosFileEditorCursorPresent sfec of
                        Nothing -> ("Empty file", fileKeyMapEmptyMatchers)
                        Just sfc ->
                          case sfc ^. smosFileCursorEntrySelectionL of
                            WholeEntrySelected -> ("Entry", fileKeyMapEntryMatchers)
                            HeaderSelected -> ("Header", fileKeyMapHeaderMatchers)
                            ContentsSelected -> ("Contents", fileKeyMapContentsMatchers)
                            TimestampsSelected -> ("Timestamps", fileKeyMapTimestampsMatchers)
                            PropertiesSelected -> ("Properties", fileKeyMapPropertiesMatchers)
                            StateHistorySelected -> ("State History", fileKeyMapStateHistoryMatchers)
                            TagsSelected -> ("Tags", fileKeyMapTagsMatchers)
                            LogbookSelected -> ("Logbook", fileKeyMapLogbookMatchers)
            BrowserSelected -> case editorCursorBrowserCursor ec of
              Nothing -> Nothing
              Just fbc ->
                let BrowserKeyMap {..} = keyMapBrowserKeyMap
                    BrowserKeyMap _ _ _ _ _ = keyMapBrowserKeyMap
                 in ( \(t, ms) ->
                        withHelpBindings t $ ms ++ browserKeyMapAnyMatchers
                    )
                      $ case fileBrowserCursorSelection fbc of
                        FileBrowserSelected -> case fileBrowserSelected fbc of
                          Nothing -> ("File Browser: Empty directory", browserKeyMapEmptyMatchers)
                          Just (_, _, InProgress _) -> ("File Browser: New file or directory in progress", browserKeyMapInProgressMatchers)
                          Just (_, _, Existent _) -> ("File Browser: Existent file or directory", browserKeyMapExistentMatchers)
                        FileBrowserFilterSelected -> ("File Browser: Filter bar", browserKeyMapFilterMatchers)
            ReportSelected ->
              case editorCursorReportCursor ec of
                Nothing -> Nothing
                Just rc ->
                  let ReportsKeyMap {..} = keyMapReportsKeyMap
                   in ( \(t, ms) -> withHelpBindings t $ ms ++ reportsKeymapAnyMatchers
                      )
                        $ case rc of
                          ReportNextActions NextActionReportCursor {..} ->
                            let NextActionReportKeyMap {..} = reportsKeymapNextActionReportKeyMap
                                NextActionReportKeyMap _ _ _ = reportsKeymapNextActionReportKeyMap
                             in (\(t, ms) -> (t, ms ++ nextActionReportAnyMatchers)) $
                                  case entryReportCursorSelection nextActionReportCursorEntryReportCursor of
                                    EntryReportSelected -> ("Next Action Report", nextActionReportMatchers)
                                    EntryReportFilterSelected -> ("Next Action Report, Search", nextActionReportSearchMatchers)
                          ReportWaiting WaitingReportCursor {..} ->
                            let WaitingReportKeyMap {..} = reportsKeymapWaitingReportKeyMap
                                WaitingReportKeyMap _ _ _ = reportsKeymapWaitingReportKeyMap
                             in (\(t, ms) -> (t, ms ++ waitingReportAnyMatchers)) $
                                  case entryReportCursorSelection waitingReportCursorEntryReportCursor of
                                    EntryReportSelected -> ("Waiting Report", waitingReportMatchers)
                                    EntryReportFilterSelected -> ("Waiting Report, Search", waitingReportSearchMatchers)
                          ReportTimestamps TimestampsReportCursor {..} ->
                            let TimestampsReportKeyMap {..} = reportsKeymapTimestampsReportKeyMap
                                TimestampsReportKeyMap _ _ _ = reportsKeymapTimestampsReportKeyMap
                             in (\(t, ms) -> (t, ms ++ timestampsReportAnyMatchers)) $
                                  case entryReportCursorSelection timestampsReportCursorEntryReportCursor of
                                    EntryReportSelected -> ("Timestamps Report", timestampsReportMatchers)
                                    EntryReportFilterSelected -> ("Timestamps Report, Search", timestampsReportSearchMatchers)
                          ReportStuck _ ->
                            let StuckReportKeyMap {..} = reportsKeymapStuckReportKeyMap
                                StuckReportKeyMap _ _ = reportsKeymapStuckReportKeyMap
                             in (\(t, ms) -> (t, ms ++ stuckReportAnyMatchers)) ("Stuck report", stuckReportMatchers)
                          ReportWork WorkReportCursor {..} ->
                            let WorkReportKeyMap {..} = reportsKeymapWorkReportKeyMap
                                WorkReportKeyMap _ _ _ = reportsKeymapWorkReportKeyMap
                             in (\(t, ms) -> (t, ms ++ workReportAnyMatchers)) $
                                  case entryReportCursorSelection workReportCursorResultEntries of
                                    EntryReportSelected -> ("Work Report", workReportMatchers)
                                    EntryReportFilterSelected -> ("Work Report, Search", workReportSearchMatchers)
            HelpSelected -> Nothing, -- Should not happen
          editorCursorSelection = HelpSelected
        }

editorCursorUpdateTime :: ZonedTime -> EditorCursor -> EditorCursor
editorCursorUpdateTime zt ec =
  ec
    { editorCursorFileCursor = smosFileEditorCursorUpdateTime zt <$> editorCursorFileCursor ec
    }

data EditorSelection
  = FileSelected
  | BrowserSelected
  | ReportSelected
  | HelpSelected
  deriving (Show, Eq, Generic)

instance Validity EditorSelection

data ReportCursor
  = ReportNextActions !NextActionReportCursor
  | ReportWaiting !WaitingReportCursor
  | ReportTimestamps !TimestampsReportCursor
  | ReportStuck !StuckReportCursor
  | ReportWork !WorkReportCursor
  deriving (Show, Eq, Generic)

instance Validity ReportCursor
