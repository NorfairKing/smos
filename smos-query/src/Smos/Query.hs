module Smos.Query
    ( smosQuery
    , module Smos.Query.Config
    ) where

import Smos.Query.Agenda
import Smos.Query.Clock
import Smos.Query.Next
import Smos.Query.OptParse
import Smos.Query.Config
import Smos.Query.OptParse.Types
import Smos.Query.Waiting

smosQuery :: IO ()
smosQuery = do
    (disp, set) <- getInstructions
    execute disp set

execute :: Dispatch -> Settings -> IO ()
execute DispatchWaiting set = waiting set
execute DispatchNext set = next set
execute (DispatchClock cs) set = clock cs set
execute (DispatchAgenda as) set = agenda as set
