{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Agenda.Types where

import Data.Validity
import GHC.Generics (Generic)

data AgendaHistoricity
  = HistoricalAgenda
  | FutureAgenda
  deriving (Show, Eq, Generic)

instance Validity AgendaHistoricity
