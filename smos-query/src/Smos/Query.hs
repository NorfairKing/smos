{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Smos.Query
  ( smosQuery
  , module Smos.Query.Config
  ) where

import Prelude (IO)

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
    DispatchEntry es -> entry es
    DispatchReport es -> report es
    DispatchWork ws -> work ws
    DispatchWaiting ws -> waiting ws
    DispatchNext ns -> next ns
    DispatchClock cs -> clock cs
    DispatchAgenda as -> agenda as
    DispatchProjects ps -> projects ps
    DispatchStuck ps -> stuck ps
    DispatchLog ss -> log ss
    DispatchStats ss -> stats ss
    DispatchTags ts -> tags ts
