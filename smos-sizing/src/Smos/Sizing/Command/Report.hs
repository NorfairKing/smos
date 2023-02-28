{-# LANGUAGE RecordWildCards #-}

module Smos.Sizing.Command.Report (sizingReport) where

import Smos.Data
import Smos.Sizing.OptParse.Types

sizingReport :: Settings -> ReportSettings -> IO ()
sizingReport s rs@ReportSettings {..} = do
  print (s, rs)
  readSmosFile reportSettingPlanFile >>= print
