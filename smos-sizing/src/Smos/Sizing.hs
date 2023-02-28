module Smos.Sizing (smosSizing) where

import Smos.Sizing.Command
import Smos.Sizing.OptParse

smosSizing :: IO ()
smosSizing = do
  Instructions d sets <- getInstructions
  case d of
    DispatchReport reportSets -> sizingReport sets reportSets
