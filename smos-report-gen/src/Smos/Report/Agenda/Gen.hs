{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Agenda.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()
import Data.Time
import Smos.Data
import Smos.Data.Gen ()
import Smos.Report.Agenda
import Smos.Report.Agenda.Types
import Smos.Report.Period.Gen ()
import Smos.Report.TimeBlock.Gen ()

instance GenValid AgendaReport where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid AgendaTodayReport where
  shrinkValid = shrinkValidStructurally
  genValid = do
    d <- genValid
    aes <- genValid
    let tsSetDay = \case
          TimestampDay _ -> TimestampDay d
          TimestampLocalTime (LocalTime _ tod) -> TimestampLocalTime (LocalTime d tod)
    let aes' = map (\ae -> ae {agendaEntryTimestamp = tsSetDay (agendaEntryTimestamp ae)}) aes
    pure AgendaTodayReport {agendaTodayReportEntries = sortAgendaEntries aes'}

instance GenValid AgendaEntry where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid AgendaHistoricity where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
