{-# LANGUAGE LambdaCase #-}

module Smos.Query
  ( smosQuery,
    module Smos.Query.Config,
  )
where

import Smos.Query.Commands
import Smos.Query.Config
import Smos.Query.OptParse
import Smos.Query.OptParse.Types

smosQuery :: SmosQueryConfig -> IO ()
smosQuery sqc = do
  Instructions disp sqc' <- getInstructions sqc
  runReaderT (execute disp) sqc'

execute :: Dispatch -> Q ()
execute =
  \case
    DispatchEntry es -> smosQueryEntry es
    DispatchReport es -> smosQueryReport es
    DispatchWork ws -> smosQueryWork ws
    DispatchWaiting ws -> smosQueryWaiting ws
    DispatchNext ns -> smosQueryNext ns
    DispatchClock cs -> smosQueryClock cs
    DispatchAgenda as -> smosQueryAgenda as
    DispatchProjects ps -> smosQueryProjects ps
    DispatchStuck ps -> smosQueryStuck ps
    DispatchLog ss -> smosQueryLog ss
    DispatchStats ss -> smosQueryStats ss
    DispatchTags ts -> smosQueryTags ts
