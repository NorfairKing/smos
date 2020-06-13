{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Agenda.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()
import Smos.Data.Gen ()
import Smos.Report.Agenda
import Smos.Report.Path.Gen ()
import Smos.Report.Period.Gen ()
import Smos.Report.TimeBlock.Gen ()

instance GenValid AgendaReport where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid AgendaTodayReport where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid AgendaEntry where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
