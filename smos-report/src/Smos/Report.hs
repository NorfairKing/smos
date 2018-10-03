module Smos.Report
    ( report
    ) where

import Smos.Report.Next
import Smos.Report.OptParse
import Smos.Report.Waiting
import Smos.Report.Clock

report :: IO ()
report = do
    (disp, set) <- getInstructions
    execute disp set

execute :: Dispatch -> Settings -> IO ()
execute DispatchWaiting set = waiting set
execute DispatchNext set = next set
execute (DispatchClock cs ) set = clock cs set
