module Smos.Server.Handler.GetApiVersion
  ( serveGetApiVersion,
  )
where

import Data.SemVer
import Smos.Server.Handler.Import

serveGetApiVersion :: ServerHandler Version
serveGetApiVersion = pure apiVersion
