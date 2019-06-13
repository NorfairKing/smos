{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Agenda.Types where

import GHC.Generics (Generic)

import Data.Validity

data AgendaHistoricity
  = HistoricalAgenda
  | FutureAgenda
  deriving (Show, Eq, Generic)

instance Validity AgendaHistoricity
