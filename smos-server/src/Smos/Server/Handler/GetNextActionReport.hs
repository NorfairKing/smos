module Smos.Server.Handler.GetNextActionReport
  ( serveGetNextActionReport,
  )
where

import Smos.Report.Next
import Smos.Server.Handler.Import

serveGetNextActionReport :: AuthCookie -> ServerHandler NextActionReport
serveGetNextActionReport (AuthCookie un) = withUserId un $ \uid ->
  streamSmosFiles uid (nextActionReportConduit Nothing)
