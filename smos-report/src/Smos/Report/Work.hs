{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Work where

import GHC.Generics (Generic)

import qualified Data.Map as M
import Data.Map (Map)
import Data.Validity
import Data.Validity.Path ()

import Cursor.Simple.Forest

import Smos.Data

import Smos.Report.Config
import Smos.Report.Filter
import Smos.Report.Path
import Smos.Report.Streaming

data WorkReport =
  WorkReport
    { workReportResultEntries :: [(RootedPath, Entry)]
    , workReportEntriesWithoutContext :: [(RootedPath, Entry)]
    }
  deriving (Show, Eq, Generic)

instance Validity WorkReport

instance Semigroup WorkReport where
  wr1 <> wr2 =
    WorkReport
      { workReportResultEntries = workReportResultEntries wr1 <> workReportResultEntries wr2
      , workReportEntriesWithoutContext =
          workReportEntriesWithoutContext wr1 <> workReportEntriesWithoutContext wr2
      }

instance Monoid WorkReport where
  mempty = WorkReport {workReportResultEntries = mempty, workReportEntriesWithoutContext = mempty}

data WorkReportContext =
  WorkReportContext
    { workReportContextCurrentContext :: Filter
    , workReportContextContexts :: Map ContextName Filter
    }
  deriving (Show, Eq, Generic)

makeWorkReport :: WorkReportContext -> RootedPath -> ForestCursor Entry -> WorkReport
makeWorkReport WorkReportContext {..} rp fc =
  let cur = forestCursorCurrent fc
      match b =
        if b
          then [(rp, cur)]
          else []
      matchesSelectedContext = filterPredicate workReportContextCurrentContext rp fc
      matchesAnyContext =
        not $ any (\f -> filterPredicate f rp fc) $ M.elems workReportContextContexts
   in WorkReport
        { workReportResultEntries = match matchesSelectedContext
        , workReportEntriesWithoutContext = match matchesAnyContext
        }
