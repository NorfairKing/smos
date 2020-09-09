module Smos.Report.Formatting where

import Data.Time

daysSince :: UTCTime -> UTCTime -> Int
daysSince now t = i
  where
    i = diffInDays now t :: Int
    diffInDays :: UTCTime -> UTCTime -> Int
    diffInDays t1 t2 = floor $ diffUTCTime t1 t2 / nominalDay
