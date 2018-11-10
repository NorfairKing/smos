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
 -}
module Smos.Actions.Utils
    ( module Smos.Actions.Utils
    , module Smos.Cursor.Contents
    , module Smos.Cursor.Entry
    , module Smos.Cursor.Header
    , module Smos.Cursor.Logbook
    , module Smos.Cursor.Report.Next
    , module Smos.Cursor.SmosFile
    , module Smos.Cursor.StateHistory
    , module Smos.Cursor.Tags
    , module Smos.Cursor.Timestamps
    ) where

import Data.Maybe
import Data.Time

import Cursor.Types

import Smos.Data

import Smos.Cursor.Contents
import Smos.Cursor.Entry
import Smos.Cursor.Header
import Smos.Cursor.Logbook
import Smos.Cursor.Report.Next
import Smos.Cursor.SmosFile
import Smos.Cursor.StateHistory
import Smos.Cursor.Tags
import Smos.Cursor.Timestamps

import Lens.Micro

import Smos.Types

modifyHeaderCursorWhenSelectedM ::
       (HeaderCursor -> Maybe HeaderCursor) -> SmosM ()
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
    modifyMContentsCursorWhenSelectedM $ \mcc -> do
        cc <- mcc
        case func cc of
            Nothing -> Just cc
            Just doucc ->
                case doucc of
                    Deleted -> Nothing
                    Updated cc' -> Just cc'

modifyContentsCursorWhenSelectedM ::
       (ContentsCursor -> Maybe ContentsCursor) -> SmosM ()
modifyContentsCursorWhenSelectedM func =
    modifyContentsCursorWhenSelected $ \cc -> fromMaybe cc $ func cc

modifyContentsCursorWhenSelected ::
       (ContentsCursor -> ContentsCursor) -> SmosM ()
modifyContentsCursorWhenSelected func =
    modifyMContentsCursorWhenSelectedM $ fmap func

modifyMContentsCursorWhenSelected ::
       (Maybe ContentsCursor -> ContentsCursor) -> SmosM ()
modifyMContentsCursorWhenSelected func =
    modifyMContentsCursorWhenSelectedM $ Just . func

modifyMContentsCursorWhenSelectedM ::
       (Maybe ContentsCursor -> Maybe ContentsCursor) -> SmosM ()
modifyMContentsCursorWhenSelectedM func =
    modifyEntryCursor $ \ec ->
        case entryCursorSelected ec of
            ContentsSelected ->
                let ec' = ec & entryCursorContentsCursorL %~ func
                 in if isNothing $ entryCursorContentsCursor ec'
                        then ec' {entryCursorSelected = WholeEntrySelected}
                        else ec'
            _ -> ec

modifyTagsCursorMD ::
       (TagsCursor -> Maybe (DeleteOrUpdate TagsCursor)) -> SmosM ()
modifyTagsCursorMD func = modifyMTagsCursorMD  (>>= func)

modifyTagsCursorD :: (TagsCursor -> DeleteOrUpdate TagsCursor) -> SmosM ()
modifyTagsCursorD func =
    modifyTagsCursorM $ \tc ->
        case func tc of
            Deleted -> Nothing
            Updated tc' -> Just tc'

modifyTagsCursorM :: (TagsCursor -> Maybe TagsCursor) -> SmosM ()
modifyTagsCursorM func = modifyTagsCursor $ \tc -> fromMaybe tc $ func tc

modifyTagsCursor :: (TagsCursor -> TagsCursor) -> SmosM ()
modifyTagsCursor func = modifyMTagsCursorM $ fmap func

modifyMTagsCursorD ::
       (Maybe TagsCursor -> DeleteOrUpdate TagsCursor) -> SmosM ()
modifyMTagsCursorD func = modifyMTagsCursorMD $ Just . func

modifyMTagsCursorMD ::
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
modifyMTagsCursorM func = modifyEntryCursor $ entryCursorTagsCursorL %~ func

modifyTimestampsCursorM ::
       (TimestampsCursor -> Maybe TimestampsCursor) -> SmosM ()
modifyTimestampsCursorM func =
    modifyTimestampsCursor $ \tsc -> fromMaybe tsc $ func tsc

modifyTimestampsCursor :: (TimestampsCursor -> TimestampsCursor) -> SmosM ()
modifyTimestampsCursor func = modifyMTimestampsCursorM $ fmap func

modifyMTimestampsCursorM ::
       (Maybe TimestampsCursor -> Maybe TimestampsCursor) -> SmosM ()
modifyMTimestampsCursorM func = modifyMTimestampsCursorSM $ pure . func

modifyMTimestampsCursorSM ::
       (Maybe TimestampsCursor -> SmosM (Maybe TimestampsCursor)) -> SmosM ()
modifyMTimestampsCursorSM func =
    modifyEntryCursorS $ entryCursorTimestampsCursorL func

modifyMTodoStateM :: (Maybe TodoState -> Maybe TodoState) -> SmosM ()
modifyMTodoStateM func =
    modifyMStateHistoryCursorSM $ \mshc -> do
        now <- liftIO getCurrentTime
        pure $
            case stateHistoryCursorModTodoState now func mshc of
                Nothing -> mshc
                Just mshc' -> Just mshc'

modifyMStateHistoryCursorSM ::
       (Maybe StateHistoryCursor -> SmosM (Maybe StateHistoryCursor))
    -> SmosM ()
modifyMStateHistoryCursorSM func =
    modifyEntryCursorS $ entryCursorStateHistoryCursorL func

modifyLogbookCursorSM ::
       (LogbookCursor -> SmosM (Maybe LogbookCursor)) -> SmosM ()
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
    modifyMFileCursorMS $ \mc ->
        case mc of
            Nothing -> pure Nothing
            Just c -> Just <$> func c

modifyMFileCursor :: (SmosFileCursor -> Maybe SmosFileCursor) -> SmosM ()
modifyMFileCursor func =
    modifyMFileCursorM $ \case
        Nothing -> Nothing
        Just sfc -> func sfc

modifyFileCursorD ::
       (SmosFileCursor -> DeleteOrUpdate SmosFileCursor) -> SmosM ()
modifyFileCursorD func =
    modifyMFileCursorM $ \msfc -> do
        sfc <- msfc
        case func sfc of
            Deleted -> Nothing
            Updated sfc' -> pure sfc'

modifyMFileCursorM :: (Maybe SmosFileCursor -> Maybe SmosFileCursor) -> SmosM ()
modifyMFileCursorM func = modifyMFileCursorMS $ pure . func

modifyMFileCursorMS ::
       (Maybe SmosFileCursor -> SmosM (Maybe SmosFileCursor)) -> SmosM ()
modifyMFileCursorMS func =
    modifyEditorCursorS $ editorCursorSmosFileCursorL func

modifyHelpCursorM :: (HelpCursor -> Maybe HelpCursor) -> SmosM ()
modifyHelpCursorM func = modifyHelpCursor $ \hc -> fromMaybe hc $ func hc

modifyHelpCursor :: (HelpCursor -> HelpCursor) -> SmosM ()
modifyHelpCursor func = modifyMHelpCursorM $ fmap func

modifyMHelpCursorM :: (Maybe HelpCursor -> Maybe HelpCursor) -> SmosM ()
modifyMHelpCursorM func = modifyMHelpCursorMS $ pure . func

modifyMHelpCursorMS ::
       (Maybe HelpCursor -> SmosM (Maybe HelpCursor)) -> SmosM ()
modifyMHelpCursorMS func = modifyEditorCursorS $ editorCursorHelpCursorL func

modifyNextActionReportCursorM ::
       (NextActionReportCursor -> Maybe NextActionReportCursor) -> SmosM ()
modifyNextActionReportCursorM func =
    modifyNextActionReportCursor $ \hc -> fromMaybe hc $ func hc

modifyNextActionReportCursor ::
       (NextActionReportCursor -> NextActionReportCursor) -> SmosM ()
modifyNextActionReportCursor func = modifyNextActionReportCursorS $ pure . func

modifyNextActionReportCursorS ::
       (NextActionReportCursor -> SmosM NextActionReportCursor) -> SmosM ()
modifyNextActionReportCursorS func =
    modifyReportCursorS $ \rc ->
        case rc of
            ReportNextActions narc -> ReportNextActions <$> func narc

modifyReportCursorM :: (ReportCursor -> Maybe ReportCursor) -> SmosM ()
modifyReportCursorM func = modifyReportCursor $ \hc -> fromMaybe hc $ func hc

modifyReportCursor :: (ReportCursor -> ReportCursor) -> SmosM ()
modifyReportCursor func = modifyMReportCursorM $ fmap func

modifyMReportCursorM :: (Maybe ReportCursor -> Maybe ReportCursor) -> SmosM ()
modifyMReportCursorM func = modifyMReportCursorMS $ pure . func

modifyReportCursorS :: (ReportCursor -> SmosM ReportCursor) -> SmosM ()
modifyReportCursorS func =
    modifyMReportCursorMS $ \mrc ->
        case mrc of
            Nothing -> pure Nothing
            Just rc -> Just <$> func rc

modifyMReportCursorMS ::
       (Maybe ReportCursor -> SmosM (Maybe ReportCursor)) -> SmosM ()
modifyMReportCursorMS func =
    modifyEditorCursorS $ editorCursorReportCursorL func

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
