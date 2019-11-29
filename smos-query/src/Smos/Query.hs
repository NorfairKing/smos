{-# LANGUAGE NoImplicitPrelude #-}

module Smos.Query
  ( smosQuery
  , module Smos.Query.Config
  ) where

import Prelude (IO)

import Smos.Query.Agenda
import Smos.Query.Clock
import Smos.Query.Config
import Smos.Query.Entry
import Smos.Query.Log
import Smos.Query.Next
import Smos.Query.OptParse
import Smos.Query.OptParse.Types
import Smos.Query.Projects
import Smos.Query.Stats
import Smos.Query.Tags
import Smos.Query.Waiting
import Smos.Query.Work

smosQuery :: SmosQueryConfig -> IO ()
smosQuery sqc = do
  Instructions disp sqc' <- getInstructions sqc
  runReaderT (execute disp) sqc'

execute :: Dispatch -> Q ()
execute (DispatchEntry es) = entry es
execute (DispatchWork ws) = work ws
execute (DispatchWaiting ws) = waiting ws
execute (DispatchNext ns) = next ns
execute (DispatchClock cs) = clock cs
execute (DispatchAgenda as) = agenda as
execute (DispatchProjects ps) = projects ps
execute (DispatchLog ss) = log ss
execute (DispatchStats ss) = stats ss
execute (DispatchTags ts) = tags ts
