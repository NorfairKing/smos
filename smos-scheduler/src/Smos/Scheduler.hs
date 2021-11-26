module Smos.Scheduler
  ( smosScheduler,
  )
where

import Smos.Scheduler.Commands
import Smos.Scheduler.OptParse

smosScheduler :: IO ()
smosScheduler = getInstructions >>= scheduler

scheduler :: Instructions -> IO ()
scheduler (Instructions d s) = case d of
  DispatchCheck -> check s
  DispatchNext -> next s
  DispatchSchedule -> schedule s
  DispatchSample f mdpt -> sample s f mdpt
