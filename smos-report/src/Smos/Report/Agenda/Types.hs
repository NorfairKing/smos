{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Agenda.Types where

import Data.Validity
import GHC.Generics (Generic)
import OptEnvConf

data AgendaHistoricity
  = HistoricalAgenda
  | FutureAgenda
  deriving (Show, Generic)

instance Validity AgendaHistoricity

instance HasParser AgendaHistoricity where
  settingsParser =
    withDefault HistoricalAgenda $
      choice
        [ setting
            [ help "Select all entries",
              switch HistoricalAgenda,
              long "historical"
            ],
          setting
            [ help "Select only entries in the future",
              switch FutureAgenda,
              long "future"
            ]
        ]
