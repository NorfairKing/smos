module Smos.ASCIInema
  ( smosASCIInema,
  )
where

import Smos.ASCIInema.Commands
import Smos.ASCIInema.OptParse

smosASCIInema :: IO ()
smosASCIInema = getInstructions >>= scheduler

scheduler :: Instructions -> IO ()
scheduler (Instructions d _) = case d of
  DispatchRecord f -> record f
