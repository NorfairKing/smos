{-# LANGUAGE LambdaCase #-}

module Smos.Actions.Utils
    ( module Smos.Actions.Utils
    , module Smos.Cursor.Entry
    , module Smos.Cursor.SmosFile
    ) where

import Data.Maybe

import Smos.Cursor.Entry
import Smos.Cursor.SmosFile

import Lens.Micro

import Smos.Types

modifyEntryCursor :: (EntryCursor -> EntryCursor) -> SmosM ()
modifyEntryCursor func =
    modifyFileCursor $ \sfc -> sfc & smosFileCursorSelectedEntryL %~ func

modifyEmptyFile :: SmosFileCursor -> SmosM ()
modifyEmptyFile = modifyEmptyFileS . pure

modifyEmptyFileS :: SmosM SmosFileCursor -> SmosM ()
modifyEmptyFileS func =
    modifyMFileCursorS $ \case
        Nothing -> Just <$> func
        _ -> pure Nothing

modifyFileCursorM :: (SmosFileCursor -> Maybe SmosFileCursor) -> SmosM ()
modifyFileCursorM func = modifyFileCursor $ \sfc -> fromMaybe sfc $ func sfc

modifyFileCursor :: (SmosFileCursor -> SmosFileCursor) -> SmosM ()
modifyFileCursor func = modifyMFileCursor $ Just . func

modifyFileCursorS :: (SmosFileCursor -> SmosM SmosFileCursor) -> SmosM ()
modifyFileCursorS func =
    modifyMFileCursorS $ \mc ->
        case mc of
            Nothing -> pure Nothing
            Just c -> Just <$> func c

modifyMFileCursor :: (SmosFileCursor -> Maybe SmosFileCursor) -> SmosM ()
modifyMFileCursor func =
    modifyMFileCursorM $ \case
        Nothing -> Nothing
        Just sfc -> func sfc

modifyMFileCursorM :: (Maybe SmosFileCursor -> Maybe SmosFileCursor) -> SmosM ()
modifyMFileCursorM func = modifyMFileCursorS $ pure . func

modifyMFileCursorS ::
       (Maybe SmosFileCursor -> SmosM (Maybe SmosFileCursor)) -> SmosM ()
modifyMFileCursorS func = do
    ss <- get
    let msc = smosStateCursor ss
    msc' <- func msc
    let ss' = ss {smosStateCursor = msc'}
    put ss'
