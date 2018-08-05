{-# LANGUAGE LambdaCase #-}

module Smos.Actions.Utils
    ( module Smos.Actions.Utils
    , module Smos.Cursor.SmosFile
    ) where

import Smos.Cursor.SmosFile

import Smos.Types

modifyEmptyFile :: SmosFileCursor -> SmosM ()
modifyEmptyFile = modifyEmptyFileS . pure

modifyEmptyFileS :: SmosM SmosFileCursor -> SmosM ()
modifyEmptyFileS func =
    modifyMFileCursorS $ \case
        Nothing -> Just <$> func
        _ -> pure Nothing

modifyFileCursor :: (SmosFileCursor -> SmosFileCursor) -> SmosM ()
modifyFileCursor func = modifyMFileCursor $ \mc -> func <$> mc

modifyFileCursorS :: (SmosFileCursor -> SmosM SmosFileCursor) -> SmosM ()
modifyFileCursorS func =
    modifyMFileCursorS $ \mc ->
        case mc of
            Nothing -> pure Nothing
            Just c -> Just <$> func c

modifyMFileCursor :: (Maybe SmosFileCursor -> Maybe SmosFileCursor) -> SmosM ()
modifyMFileCursor func = modifyMFileCursorS $ pure . func

modifyMFileCursorS ::
       (Maybe SmosFileCursor -> SmosM (Maybe SmosFileCursor)) -> SmosM ()
modifyMFileCursorS func = do
    ss <- get
    let msc = smosStateCursor ss
    msc' <- func msc
    let ss' = ss {smosStateCursor = msc'}
    put ss'
