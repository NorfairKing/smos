module Smos.Server.Handler.GetNextActionReport
  ( serveGetNextActionReport,
  )
where

import Smos.Report.Next
import Smos.Server.Handler.Import

serveGetNextActionReport :: AuthNCookie -> ServerHandler NextActionReport
serveGetNextActionReport ac = withUserId ac $ \uid ->
  streamSmosFiles uid HideArchive (nextActionReportConduit Nothing)
