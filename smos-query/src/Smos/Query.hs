module Smos.Query
    ( smosQuery
    , module Smos.Query.Config
    ) where

import Smos.Query.Agenda
import Smos.Query.Clock
import Smos.Query.Config
import Smos.Query.Entry
import Smos.Query.Next
import Smos.Query.OptParse
import Smos.Query.OptParse.Types
import Smos.Query.Waiting
import Smos.Query.Stats

smosQuery :: SmosQueryConfig -> IO ()
smosQuery sqc = do
    (disp, Settings) <- getInstructions
    runReaderT (execute disp) sqc

execute :: Dispatch -> Q ()
execute (DispatchEntry es) = entry es
execute (DispatchWaiting ws) = waiting ws
execute (DispatchNext ns) = next ns
execute (DispatchClock cs) = clock cs
execute (DispatchAgenda as) = agenda as
execute (DispatchStats ss) = stats ss
