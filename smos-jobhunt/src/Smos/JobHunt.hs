{-# LANGUAGE RecordWildCards #-}

module Smos.JobHunt (smosJobHunt) where

import Control.Monad.Logger
import Smos.JobHunt.Command
import Smos.JobHunt.OptParse

smosJobHunt :: IO ()
smosJobHunt = do
  Instructions d sets@Settings {..} <- getInstructions
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= setLogLevel) $ case d of
      DispatchInit initSettings -> smosJobHuntInit sets initSettings
      DispatchSendEmail sendEmailSettings -> smosJobHuntSendEmail sets sendEmailSettings
