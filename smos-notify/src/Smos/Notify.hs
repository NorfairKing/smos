module Smos.Notify where

import Smos.Notify.OptParse

smosNotify :: IO ()
smosNotify = do
  sets <- getSettings
  print sets
