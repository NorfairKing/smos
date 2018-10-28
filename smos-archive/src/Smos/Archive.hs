{-# LANGUAGE RecordWildCards #-}

module Smos.Archive
    ( archive
    ) where

import Smos.Archive.OptParse
import Smos.Archive.OptParse.Types

archive :: IO ()
archive = do
    s@Settings {..} <- getSettings
    print s
