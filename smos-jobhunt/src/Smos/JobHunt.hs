{-# LANGUAGE RecordWildCards #-}

module Smos.JobHunt (smosJobHunt) where

import Smos.CLI.Logging
import Smos.JobHunt.Command
import Smos.JobHunt.OptParse

smosJobHunt :: IO ()
smosJobHunt = do
  Instructions d sets@Settings {..} <- getInstructions
  runFilteredLogger setLogLevel $ case d of
    DispatchInit initSettings -> smosJobHuntInit sets initSettings
    DispatchSendEmail sendEmailSettings -> smosJobHuntSendEmail sets sendEmailSettings
