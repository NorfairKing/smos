{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetBookingSlots
  ( serveGetBookingSlots,
    computeBookingSlots,
  )
where

import Conduit
import qualified Data.IntervalMap.Generic.Lazy as IM
import qualified Data.Map as M
import Data.Time.Zones
import Data.Time.Zones.All
import Smos.Report.Free
import Smos.Report.Time
import Smos.Server.Handler.Import

serveGetBookingSlots :: Username -> ServerHandler BookingSlots
serveGetBookingSlots username = withUsernameId username $ \uid -> do
  mBookingConfig <- runDB $ getBy $ UniqueBookingConfigUser uid
  case mBookingConfig of
    Nothing -> throwError err404
    Just (Entity _ bookingConfig) -> computeBookingSlots uid bookingConfig

computeBookingSlots :: UserId -> BookingConfig -> ServerHandler BookingSlots
computeBookingSlots uid BookingConfig {..} = do
  userToday <- liftIO $ localDay . utcToLocalTimeTZ (tzByLabel bookingConfigTimeZone) <$> getCurrentTime
  -- TODO Make the slot configurable
  let userBegin = LocalTime (addDays 1 userToday) midnight
  let userEnd = LocalTime (addDays 15 userToday) midnight
  let careSlot =
        Slot
          { slotBegin = userBegin,
            slotEnd = userEnd
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
