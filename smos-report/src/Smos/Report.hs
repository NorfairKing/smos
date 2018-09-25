{-# LANGUAGE NoImplicitPrelude #-}

module Smos.Report
    ( report
    ) where

import Import

import Smos.Report.Next
import Smos.Report.OptParse
import Smos.Report.Wait

report :: IO ()
report = do
    (disp, set) <- getInstructions
    execute disp set

execute :: Dispatch -> Settings -> IO ()
execute DispatchWaiting set = wait set
execute DispatchNext set = next set
