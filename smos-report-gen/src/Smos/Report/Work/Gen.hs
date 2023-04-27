{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Work.Gen where

import Cursor.Forest.Gen ()
import Data.DList (DList)
import qualified Data.DList as DList
import Data.GenValidity
import Data.GenValidity.Path ()
import Smos.Data.Gen ()
import Smos.Report.Agenda
import Smos.Report.Agenda.Gen ()
import Smos.Report.Filter.Gen ()
import Smos.Report.OptParse.Gen ()
import Smos.Report.Period.Gen ()
import Smos.Report.Sorter.Gen ()
import Smos.Report.Stuck.Gen ()
import Smos.Report.TimeBlock.Gen ()
import Smos.Report.Waiting.Gen ()
import Smos.Report.Work

instance GenValid a => GenValid (DList a) where
  genValid = DList.fromList <$> genValid
  shrinkValid = fmap DList.fromList . shrinkValid . DList.toList

instance GenValid WorkReportContext where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid IntermediateWorkReport where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid WorkReport where
  genValid =
    WorkReport
      <$> genValid
      <*> (sortAgendaEntries <$> genValid)
      <*> genValid
      <*> genValid
      <*> genValid
      <*> genValid
      <*> genValid
      <*> genValid
  shrinkValid = shrinkValidStructurally
