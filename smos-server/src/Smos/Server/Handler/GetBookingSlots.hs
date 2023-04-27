module Smos.Server.Handler.GetBookingSlots
  ( serveGetBookingSlots,
    computeBookingSlots,
  )
where

import Conduit
import qualified Data.IntervalMap.Generic.Lazy as IM
import qualified Data.Map as M
import Smos.Report.Free
import Smos.Report.Time
import Smos.Server.Handler.Import

serveGetBookingSlots :: Username -> ServerHandler BookingSlots
serveGetBookingSlots username = withUsernameId username $ \uid -> do
  computeBookingSlots uid

computeBookingSlots :: UserId -> ServerHandler BookingSlots
computeBookingSlots uid = do
  -- TODO use user timezone to figure out when today is so that we only book slots in the future.
  today <- liftIO $ utctDay <$> getCurrentTime
  let endDay = addDays 14 today
  -- Make the slot configurable
  let careSlot =
        Slot
          { slotBegin = LocalTime today midnight,
            slotEnd = LocalTime endDay midnight
          }
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
    . concatMap (splitSlots (30 * 60))
    . map (\s -> (slotBegin s, slotDuration s))
    . map fst
    . concatMap IM.toList
    . map (unFreeMap . snd)
    . M.toList
    . unFreeReport

splitSlots :: NominalDiffTime -> (LocalTime, NominalDiffTime) -> [(LocalTime, NominalDiffTime)]
splitSlots chunkSize = go
  where
    go (begin, totalDuration) =
      if totalDuration >= 2 * chunkSize
        then (begin, chunkSize) : go (addLocalTime chunkSize begin, totalDuration - chunkSize)
        else [(begin, chunkSize)]
