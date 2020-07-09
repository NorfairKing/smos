module Smos.ASCIInema.Commands.Record
  ( record,
  )
where

import Path
import Path.IO

record :: Path Abs File -> IO ()
record f = do
  pure ()
