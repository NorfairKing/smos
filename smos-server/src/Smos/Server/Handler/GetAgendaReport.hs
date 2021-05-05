module Smos.Server.Handler.GetAgendaReport
  ( serveGetAgendaReport,
  )
where

import Conduit
import Data.Time
import Smos.Report.Agenda
import Smos.Report.Agenda.Types
import Smos.Report.Period
import Smos.Report.TimeBlock
import Smos.Server.Handler.Import

serveGetAgendaReport :: AuthNCookie -> ServerHandler AgendaReport
serveGetAgendaReport ac = withUserId ac $ \uid -> do
  now <- liftIO getZonedTime
  streamSmosFiles uid HideArchive (agendaReportConduit now AllTime OneBlock HistoricalAgenda Nothing)
