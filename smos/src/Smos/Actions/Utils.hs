{-# LANGUAGE LambdaCase #-}

module Smos.Actions.Utils
    ( module Smos.Actions.Utils
    , module Smos.Cursor.SmosFile
    ) where

import Smos.Cursor.SmosFile

import Smos.Types

modifyEmptyFile :: SmosM SmosFileCursor -> SmosM ()
modifyEmptyFile func =
    modifyMFileCursorS $ \case
        Nothing -> Just <$> func
        _ -> pure Nothing

modifyMFileCursorS ::
       (Maybe SmosFileCursor -> SmosM (Maybe SmosFileCursor)) -> SmosM ()
modifyMFileCursorS func = do
    ss <- get
    let msc = smosStateCursor ss
    msc' <- func msc
    let ss' = ss {smosStateCursor = msc'}
    put ss'
