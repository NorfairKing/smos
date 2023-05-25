{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetBookingSlots
  ( serveGetBookingSlots,
    computeBookingSlots,
  )
where

import Conduit
import qualified Data.IntervalMap.Generic.Lazy as IM
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time.Zones
import Data.Time.Zones.All
import Data.Word
import Smos.Report.Free
import Smos.Report.Time
import Smos.Server.Booking (getUserBookingSettings)
import Smos.Server.Handler.Import

serveGetBookingSlots :: Username -> Word8 -> ServerHandler BookingSlots
serveGetBookingSlots username slotSize = withUsernameId username $ \uid -> do
  mBookingSettings <- getUserBookingSettings uid
  case mBookingSettings of
    Nothing -> throwError err404
    Just bookingSettings ->
      if slotSize `S.member` bookingSettingAllowedDurations bookingSettings
        then computeBookingSlots uid slotSize bookingSettings
        else throwError $ err400 {errBody = "This duration is not allowed."}

computeBookingSlots :: UserId -> Word8 -> BookingSettings -> ServerHandler BookingSlots
computeBookingSlots uid slotSize BookingSettings {..} = do
  userToday <- liftIO $ localDay . utcToLocalTimeTZ (tzByLabel bookingSettingTimeZone) <$> getCurrentTime
  let userBegin = LocalTime (addDays (toInteger bookingSettingMinimumDaysAhead) userToday) midnight
  let userEnd = LocalTime (addDays (toInteger bookingSettingMaximumDaysAhead) userToday) midnight
  let careSlot =
        Slot
          { slotBegin = userBegin,
            slotEnd = userEnd
          }
  freeReportToBookingSlots slotSize . filterFreeReportByAllowedDays bookingSettingAllowedDays
    <$> streamSmosFiles
      uid
      Don'tHideArchive
      ( freeReportConduit
          careSlot
          (Just (Minutes ((fromIntegral :: Word8 -> Word) slotSize)))
          (Just bookingSettingEarliestTimeOfDay)
          (Just bookingSettingLatestTimeOfDay)
      )

filterFreeReportByAllowedDays :: Set DayOfWeek -> FreeReport -> FreeReport
filterFreeReportByAllowedDays allowedDays (FreeReport fm) =
  FreeReport $ M.filterWithKey (\d _ -> dayOfWeek d `S.member` allowedDays) fm

-- TODO consider putting this in smos-report and giving smos-query access.
freeReportToBookingSlots :: Word8 -> FreeReport -> BookingSlots
freeReportToBookingSlots slotSize =
  BookingSlots
    . M.fromList
    . concatMap (splitSlots slotSize)
    . map (\s -> (slotBegin s, slotDuration s))
    . map fst
    . concatMap IM.toList
    . map (unFreeMap . snd)
    . M.toList
    . unFreeReport

splitSlots :: Word8 -> (LocalTime, NominalDiffTime) -> [(LocalTime, NominalDiffTime)]
splitSlots slotSize = go
  where
    chunkSize = fromIntegral slotSize * 60
    go (begin, totalDuration) =
      if totalDuration >= 2 * chunkSize
        then (begin, chunkSize) : go (addLocalTime chunkSize begin, totalDuration - chunkSize)
        else [(begin, chunkSize)]
