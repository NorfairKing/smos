module Smos.Convert.Org
    ( convertOrg
    ) where

import Smos.Convert.Org.OptParse
convertOrg :: IO ()
convertOrg = do
    sets <- getSettings
    print sets
