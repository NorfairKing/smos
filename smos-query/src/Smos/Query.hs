{-# LANGUAGE NoImplicitPrelude #-}

module Smos.Query
    ( smosQuery
    , module Smos.Query.Config
    ) where

import Prelude (IO)

import Smos.Query.Agenda
import Smos.Query.Clock
import Smos.Query.Config
import Smos.Query.Default
import Smos.Query.Entry
import Smos.Query.Log
import Smos.Query.Next
import Smos.Query.OptParse
import Smos.Query.OptParse.Types
import Smos.Query.Projects
import Smos.Query.Stats
import Smos.Query.Waiting

smosQuery :: IO ()
smosQuery = do
    (disp, stt) <- getInstructions
    let sqc = smosQueryConfig (sttWorkflowDir stt)
    runReaderT (execute disp) sqc

execute :: Dispatch -> Q ()
execute (DispatchEntry es) = entry es
execute (DispatchWaiting ws) = waiting ws
execute (DispatchNext ns) = next ns
execute (DispatchClock cs) = clock cs
execute (DispatchAgenda as) = agenda as
execute DispatchProjects = projects
execute (DispatchLog ss) = log ss
execute (DispatchStats ss) = stats ss
