module Smos.Server.Handler.GetBookingSlots
  ( serveGetBookingSlots,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.DirForest (DirForest)
import qualified Data.DirForest as DF
import Data.IntervalMap.Generic.Lazy (IntervalMap)
import qualified Data.IntervalMap.Generic.Lazy as IM
import qualified Data.Map as M
import Path
import Smos.Data
import Smos.Report.Free
import Smos.Report.Time
import Smos.Server.Handler.Import

serveGetBookingSlots :: Username -> ServerHandler BookingSlots
serveGetBookingSlots username = withUsernameId username $ \uid -> do
  -- TODO use user timezone to figure out when today is so that we only book slots in the future.
  today <- liftIO $ utctDay <$> getCurrentTime
  let endDay = addDays 14 today
  -- Make the slot configurable
  let careSlot = Slot {slotBegin = LocalTime today midnight, slotEnd = LocalTime endDay midnight}
  freeReportToBookingSlots
    <$> streamSmosFiles
      uid
      Don'tHideArchive
      ( freeReportConduit
          careSlot
          (Just (Minutes 30))
          (Just (TimeOfDay 09 00 00))
          (Just (TimeOfDay 17 00 00))
      )

-- TODO consider putting this in smos-report and giving smos-query access.
freeReportToBookingSlots :: FreeReport -> BookingSlots
freeReportToBookingSlots =
  BookingSlots
    . M.fromList
    . map (\s -> (slotBegin s, slotDuration s))
    . map fst
    . concatMap IM.toList
    . map (unFreeMap . snd)
    . M.toList
    . unFreeReport
