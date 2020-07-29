{-# LANGUAGE LambdaCase #-}

{-
 - Cheatsheet:
 -
 - modifyMXXXM :: (Maybe X -> Maybe X) -> SmosM ()                   -- Modify a Maybe field in SmosM
 - modifyMXXXSM :: (Maybe X -> SmosM (Maybe X)) -> SmosM ()          -- Modify a Maybe field in SmosM
 - modifyMXXXD :: (Maybe X -> DeleteOrUpdate X) -> SmosM ()          -- Modify a Maybe field purely,  possibly delete
 - modifyMXXXMD :: (Maybe X -> Maybe (DeleteOrUpdate X)) -> SmosM () -- Modify a Maybe field purely, don't do anything if 'Nothing', possibly delete
 - modifyXXXMD :: (X -> Maybe (DeleteOrUpdate X)) -> SmosM ()        -- Modify purely, don't do anything if 'Nothing', possibly delete
 - modifyXXXM :: (X -> Maybe X) -> SmosM ()                          -- Modify purely, don't do anything if 'Nothing'
 - modifyXXXD :: (X -> DeleteOrUpdate X) -> SmosM ()                 -- Modify purely, possibly delete
 - modifyXXX :: (X -> X) -> SmosM ()                                 -- Modify purely
 - modifyXXXS :: (X -> S X) -> SmosM ()                              -- Modify in SmosM
 - modifyXXXRaw :: (Y -> Y) -> SmosM ()                            -- Modify the raw field in SmosM
 - modifyXXXRawS :: (Y -> S Y) -> SmosM ()                            -- Modify the raw field in SmosM
 -}
module Smos.Actions.Utils
  ( module Smos.Actions.Utils,
    module Smos.Cursor.Contents,
    module Smos.Cursor.Entry,
    module Smos.Cursor.Header,
    module Smos.Cursor.Logbook,
    module Smos.Cursor.Properties,
    module Smos.Cursor.Report.Next,
    module Smos.Cursor.SmosFile,
    module Smos.Cursor.StateHistory,
    module Smos.Cursor.Tags,
    module Smos.Cursor.Timestamps,
  )
where

import Cursor.Types
import Data.Maybe
import Data.Time
import Lens.Micro
import Smos.Cursor.Contents
import Smos.Cursor.Entry
import Smos.Cursor.FileBrowser
import Smos.Cursor.Header
import Smos.Cursor.Logbook
import Smos.Cursor.Properties
import Smos.Cursor.Report.Next
import Smos.Cursor.SmosFile
import Smos.Cursor.SmosFileEditor
import Smos.Cursor.StateHistory
import Smos.Cursor.Tags
import Smos.Cursor.Timestamps
import Smos.Data
import Smos.History
import Smos.Types

modifyHeaderCursorWhenSelectedMD ::
  (HeaderCursor -> Maybe (DeleteOrUpdate HeaderCursor)) -> SmosM ()
modifyHeaderCursorWhenSelectedMD func = modifyHeaderCursorWhenSelectedM $ dullMDelete . func

modifyHeaderCursorWhenSelectedM :: (HeaderCursor -> Maybe HeaderCursor) -> SmosM ()
modifyHeaderCursorWhenSelectedM func =
  modifyHeaderCursorWhenSelected $ \hc -> fromMaybe hc $ func hc

modifyHeaderCursorWhenSelected :: (HeaderCursor -> HeaderCursor) -> SmosM ()
modifyHeaderCursorWhenSelected func =
  modifyEntryCursor $ \ec ->
    case entryCursorSelected ec of
      HeaderSelected -> ec & entryCursorHeaderCursorL %~ func
      _ -> ec

modifyContentsCursorWhenSelectedDM ::
  (ContentsCursor -> Maybe (DeleteOrUpdate ContentsCursor)) -> SmosM ()
modifyContentsCursorWhenSelectedDM func =
  modifyMContentsCursorRaw $ \mcc -> do
    cc <- mcc
    case func cc of
      Nothing -> Just cc
      Just doucc ->
        case doucc of
          Deleted -> Nothing
          Updated cc' -> Just cc'

modifyContentsCursorWhenSelectedM :: (ContentsCursor -> Maybe ContentsCursor) -> SmosM ()
modifyContentsCursorWhenSelectedM func =
  modifyContentsCursorWhenSelected $ \cc -> fromMaybe cc $ func cc

modifyContentsCursorWhenSelected :: (ContentsCursor -> ContentsCursor) -> SmosM ()
modifyContentsCursorWhenSelected func = modifyMContentsCursorWhenSelectedM $ fmap func

modifyMContentsCursorWhenSelectedM :: (Maybe ContentsCursor -> Maybe ContentsCursor) -> SmosM ()
modifyMContentsCursorWhenSelectedM func =
  modifyMContentsCursorRaw $ \mcc ->
    case func mcc of
      Nothing -> mcc
      Just mcc' -> Just mcc'

modifyMContentsCursorWhenSelected :: (Maybe ContentsCursor -> ContentsCursor) -> SmosM ()
modifyMContentsCursorWhenSelected func = modifyMContentsCursorWhenSelectedM $ Just . func

modifyMContentsCursorRaw :: (Maybe ContentsCursor -> Maybe ContentsCursor) -> SmosM ()
modifyMContentsCursorRaw func = modifyMContentsCursorRawS $ pure . func

modifyMContentsCursorRawS :: (Maybe ContentsCursor -> SmosM (Maybe ContentsCursor)) -> SmosM ()
modifyMContentsCursorRawS func =
  modifyEntryCursorS $ \ec ->
    case entryCursorSelected ec of
      ContentsSelected -> do
        let mcc = entryCursorContentsCursor ec
        mcc' <- func mcc
        let ec' = ec {entryCursorContentsCursor = mcc'}
        pure $
          if isNothing mcc'
            then ec' {entryCursorSelected = WholeEntrySelected}
            else ec'
      _ -> pure ec

modifyTagsCursorMD :: (TagsCursor -> Maybe (DeleteOrUpdate TagsCursor)) -> SmosM ()
modifyTagsCursorMD func = modifyMTagsCursorMD (>>= func)

modifyTagsCursorD :: (TagsCursor -> DeleteOrUpdate TagsCursor) -> SmosM ()
modifyTagsCursorD func =
  -- TODO this is wrong
  modifyTagsCursorM $ \tc ->
    case func tc of
      Deleted -> Nothing
      Updated tc' -> Just tc'

modifyTagsCursorM :: (TagsCursor -> Maybe TagsCursor) -> SmosM ()
modifyTagsCursorM func = modifyTagsCursor $ \tc -> fromMaybe tc $ func tc

modifyTagsCursor :: (TagsCursor -> TagsCursor) -> SmosM ()
modifyTagsCursor func = modifyMTagsCursorM $ fmap func

modifyMTagsCursorD :: (Maybe TagsCursor -> DeleteOrUpdate TagsCursor) -> SmosM ()
modifyMTagsCursorD func = modifyMTagsCursorMD $ Just . func

modifyMTagsCursorMD :: -- TODO this is wrong
  (Maybe TagsCursor -> Maybe (DeleteOrUpdate TagsCursor)) -> SmosM ()
modifyMTagsCursorMD func =
  modifyMTagsCursorM $ \mtc ->
    case func mtc of
      Nothing -> Nothing
      Just Deleted -> Nothing
      Just (Updated tc') -> Just tc'

modifyMTagsCursor :: (Maybe TagsCursor -> TagsCursor) -> SmosM ()
modifyMTagsCursor func = modifyMTagsCursorM $ Just . func

modifyMTagsCursorM :: (Maybe TagsCursor -> Maybe TagsCursor) -> SmosM ()
modifyMTagsCursorM func =
  modifyEntryCursor $ \ec ->
    ec
      & case func (entryCursorTagsCursor ec) of
        Nothing -> (entryCursorSelectionL .~ WholeEntrySelected) . (entryCursorTagsCursorL .~ Nothing)
        Just tsc -> entryCursorTagsCursorL ?~ tsc

modifyPropertiesCursorM :: (PropertiesCursor -> Maybe PropertiesCursor) -> SmosM ()
modifyPropertiesCursorM func = modifyPropertiesCursor $ \tsc -> fromMaybe tsc $ func tsc

modifyPropertiesCursor :: (PropertiesCursor -> PropertiesCursor) -> SmosM ()
modifyPropertiesCursor func = modifyMPropertiesCursorM $ fmap func

modifyPropertiesCursorMD ::
  (PropertiesCursor -> Maybe (DeleteOrUpdate PropertiesCursor)) -> SmosM ()
modifyPropertiesCursorMD func =
  modifyMPropertiesCursorM $ \mpc -> do
    pc <- mpc
    case func pc of
      Nothing -> pure pc
      Just Deleted -> Nothing
      Just (Updated pc') -> Just pc'

modifyMPropertiesCursorM :: (Maybe PropertiesCursor -> Maybe PropertiesCursor) -> SmosM ()
modifyMPropertiesCursorM func = modifyMPropertiesCursorSM $ pure . func

modifyMPropertiesCursorSM :: (Maybe PropertiesCursor -> SmosM (Maybe PropertiesCursor)) -> SmosM ()
modifyMPropertiesCursorSM func = modifyEntryCursorS $ entryCursorPropertiesCursorL func

modifyTimestampsCursorM :: (TimestampsCursor -> Maybe TimestampsCursor) -> SmosM ()
modifyTimestampsCursorM func = modifyTimestampsCursor $ \tsc -> fromMaybe tsc $ func tsc

modifyTimestampsCursor :: (TimestampsCursor -> TimestampsCursor) -> SmosM ()
modifyTimestampsCursor func = modifyMTimestampsCursorM $ fmap func

modifyMTimestampsCursorM :: (Maybe TimestampsCursor -> Maybe TimestampsCursor) -> SmosM ()
modifyMTimestampsCursorM func = modifyMTimestampsCursorSM $ pure . func

modifyMTimestampsCursorSM :: (Maybe TimestampsCursor -> SmosM (Maybe TimestampsCursor)) -> SmosM ()
modifyMTimestampsCursorSM func = modifyEntryCursorS $ entryCursorTimestampsCursorL func

modifyMTodoStateM :: (Maybe TodoState -> Maybe TodoState) -> SmosM ()
modifyMTodoStateM func =
  modifyMStateHistoryCursorSM $ \mshc -> do
    now <- liftIO getCurrentTime
    pure $
      case stateHistoryCursorModTodoState now func mshc of
        Nothing -> mshc
        Just mshc' -> Just mshc'

modifyMStateHistoryCursorSM ::
  (Maybe StateHistoryCursor -> SmosM (Maybe StateHistoryCursor)) -> SmosM ()
modifyMStateHistoryCursorSM func = modifyEntryCursorS $ entryCursorStateHistoryCursorL func

modifyLogbookCursorSM :: (LogbookCursor -> SmosM (Maybe LogbookCursor)) -> SmosM ()
modifyLogbookCursorSM func =
  modifyLogbookCursorS $ \lbc -> do
    mlbc <- func lbc
    pure $ fromMaybe lbc mlbc

modifyLogbookCursorS :: (LogbookCursor -> SmosM LogbookCursor) -> SmosM ()
modifyLogbookCursorS func = modifyEntryCursorS $ entryCursorLogbookCursorL func

modifyEntryCursor :: (EntryCursor -> EntryCursor) -> SmosM ()
modifyEntryCursor func = modifyEntryCursorS $ pure . func

modifyEntryCursorS :: (EntryCursor -> SmosM EntryCursor) -> SmosM ()
modifyEntryCursorS func = modifyFileCursorS $ smosFileCursorSelectedEntryL func

modifyEmptyFile :: SmosFileCursor -> SmosM ()
modifyEmptyFile = modifyEmptyFileS . pure

modifyEmptyFileS :: SmosM SmosFileCursor -> SmosM ()
modifyEmptyFileS func =
  modifyMFileCursorMS $ \case
    Nothing -> Just <$> func
    _ -> pure Nothing

modifyFileCursorM :: (SmosFileCursor -> Maybe SmosFileCursor) -> SmosM ()
modifyFileCursorM func = modifyFileCursor $ \sfc -> fromMaybe sfc $ func sfc

modifyFileCursor :: (SmosFileCursor -> SmosFileCursor) -> SmosM ()
modifyFileCursor func = modifyMFileCursor $ Just . func

modifyFileCursorS :: (SmosFileCursor -> SmosM SmosFileCursor) -> SmosM ()
modifyFileCursorS func =
  modifyMFileCursorMS $ \case
    Nothing -> pure Nothing
    Just c -> Just <$> func c

modifyMFileCursor :: (SmosFileCursor -> Maybe SmosFileCursor) -> SmosM ()
modifyMFileCursor func =
  modifyMFileCursorM $ \case
    Nothing -> Nothing
    Just sfc -> func sfc

modifyFileCursorD :: (SmosFileCursor -> DeleteOrUpdate SmosFileCursor) -> SmosM ()
modifyFileCursorD func =
  modifyMFileCursorM $ \msfc -> do
    sfc <- msfc
    case func sfc of
      Deleted -> Nothing
      Updated sfc' -> pure sfc'

modifyMFileCursorM :: (Maybe SmosFileCursor -> Maybe SmosFileCursor) -> SmosM ()
modifyMFileCursorM func = modifyMFileCursorMS $ pure . func

modifyMFileCursorMS :: (Maybe SmosFileCursor -> SmosM (Maybe SmosFileCursor)) -> SmosM ()
modifyMFileCursorMS func = modifyMFileCursorMHistoryS $ historyModM func -- Record history

unrecordFileCursorHistory :: SmosM ()
unrecordFileCursorHistory = modifyMFileCursorMHistoryM historyForgetLatest

modifyMFileCursorMHistoryM :: (History (Maybe SmosFileCursor) -> Maybe (History (Maybe SmosFileCursor))) -> SmosM ()
modifyMFileCursorMHistoryM func = modifyMFileCursorMHistory $ \h -> fromMaybe h $ func h

modifyMFileCursorMHistory :: (History (Maybe SmosFileCursor) -> History (Maybe SmosFileCursor)) -> SmosM ()
modifyMFileCursorMHistory func = modifyMFileCursorMHistoryS $ pure . func

modifyMFileCursorMHistoryS :: (History (Maybe SmosFileCursor) -> SmosM (History (Maybe SmosFileCursor))) -> SmosM ()
modifyMFileCursorMHistoryS func = modifySmosFileEditorCursorS $ smosFileEditorCursorHistoryL func

modifySmosFileEditorCursorS :: (SmosFileEditorCursor -> SmosM SmosFileEditorCursor) -> SmosM ()
modifySmosFileEditorCursorS func = modifyMSmosFileEditorCursorMS $ mapM $ \sfec -> do
  sfec' <- func sfec
  pure $
    sfec'
      { smosFileEditorUnsavedChanges = smosFileEditorUnsavedChanges sfec || (rebuildSmosFileEditorCursor sfec /= rebuildSmosFileEditorCursor sfec')
      }

modifyMSmosFileEditorCursorMS :: (Maybe SmosFileEditorCursor -> SmosM (Maybe SmosFileEditorCursor)) -> SmosM ()
modifyMSmosFileEditorCursorMS func = modifyEditorCursorS $ editorCursorFileCursorL func

modifyFileBrowserCursorM :: (FileBrowserCursor -> Maybe FileBrowserCursor) -> SmosM ()
modifyFileBrowserCursorM func = modifyFileBrowserCursor $ \hc -> fromMaybe hc $ func hc

modifyFileBrowserCursor :: (FileBrowserCursor -> FileBrowserCursor) -> SmosM ()
modifyFileBrowserCursor func = modifyMFileBrowserCursorM $ fmap func

modifyFileBrowserCursorSM :: (FileBrowserCursor -> Maybe (SmosM FileBrowserCursor)) -> SmosM ()
modifyFileBrowserCursorSM func = modifyFileBrowserCursorS $ \fbc -> fromMaybe (pure fbc) (func fbc)

modifyFileBrowserCursorS :: (FileBrowserCursor -> SmosM FileBrowserCursor) -> SmosM ()
modifyFileBrowserCursorS func = modifyMFileBrowserCursorMS $ mapM func

modifyMFileBrowserCursorM :: (Maybe FileBrowserCursor -> Maybe FileBrowserCursor) -> SmosM ()
modifyMFileBrowserCursorM func = modifyMFileBrowserCursorMS $ pure . func

modifyMFileBrowserCursorMS :: (Maybe FileBrowserCursor -> SmosM (Maybe FileBrowserCursor)) -> SmosM ()
modifyMFileBrowserCursorMS func = modifyEditorCursorS $ editorCursorBrowserCursorL func

modifyHelpCursorM :: (HelpCursor -> Maybe HelpCursor) -> SmosM ()
modifyHelpCursorM func = modifyHelpCursor $ \hc -> fromMaybe hc $ func hc

modifyHelpCursor :: (HelpCursor -> HelpCursor) -> SmosM ()
modifyHelpCursor func = modifyMHelpCursorM $ fmap func

modifyMHelpCursorM :: (Maybe HelpCursor -> Maybe HelpCursor) -> SmosM ()
modifyMHelpCursorM func = modifyMHelpCursorMS $ pure . func

modifyMHelpCursorMS :: (Maybe HelpCursor -> SmosM (Maybe HelpCursor)) -> SmosM ()
modifyMHelpCursorMS func = modifyEditorCursorS $ editorCursorHelpCursorL func

modifyNextActionReportCursorM ::
  (NextActionReportCursor -> Maybe NextActionReportCursor) -> SmosM ()
modifyNextActionReportCursorM func = modifyNextActionReportCursor $ \hc -> fromMaybe hc $ func hc

modifyNextActionReportCursor :: (NextActionReportCursor -> NextActionReportCursor) -> SmosM ()
modifyNextActionReportCursor func = modifyNextActionReportCursorS $ pure . func

modifyNextActionReportCursorS ::
  (NextActionReportCursor -> SmosM NextActionReportCursor) -> SmosM ()
modifyNextActionReportCursorS func =
  modifyReportCursorS $ \case
    ReportNextActions narc -> ReportNextActions <$> func narc

modifyReportCursorM :: (ReportCursor -> Maybe ReportCursor) -> SmosM ()
modifyReportCursorM func = modifyReportCursor $ \hc -> fromMaybe hc $ func hc

modifyReportCursor :: (ReportCursor -> ReportCursor) -> SmosM ()
modifyReportCursor func = modifyMReportCursorM $ fmap func

modifyMReportCursorM :: (Maybe ReportCursor -> Maybe ReportCursor) -> SmosM ()
modifyMReportCursorM func = modifyMReportCursorMS $ pure . func

modifyReportCursorS :: (ReportCursor -> SmosM ReportCursor) -> SmosM ()
modifyReportCursorS func =
  modifyMReportCursorMS $ \case
    Nothing -> pure Nothing
    Just rc -> Just <$> func rc

modifyMReportCursorMS :: (Maybe ReportCursor -> SmosM (Maybe ReportCursor)) -> SmosM ()
modifyMReportCursorMS func = modifyEditorCursorS $ editorCursorReportCursorL func

modifyEditorCursorM :: (EditorCursor -> Maybe EditorCursor) -> SmosM ()
modifyEditorCursorM func = modifyEditorCursor $ \ec -> fromMaybe ec $ func ec

modifyEditorCursor :: (EditorCursor -> EditorCursor) -> SmosM ()
modifyEditorCursor func = modifyEditorCursorS $ pure . func

modifyEditorCursorS :: (EditorCursor -> SmosM EditorCursor) -> SmosM ()
modifyEditorCursorS func = do
  ss <- get
  let msc = smosStateCursor ss
  msc' <- func msc
  let ss' = ss {smosStateCursor = msc'}
  put ss'
