module Smos.ASCIInema
  ( smosASCIInema,
  )
where

import Smos.ASCIInema.Commands
import Smos.ASCIInema.OptParse

smosASCIInema :: IO ()
smosASCIInema = getInstructions >>= asciinema

asciinema :: Instructions -> IO ()
asciinema (Instructions d _) = case d of
  DispatchRecord f -> record f
