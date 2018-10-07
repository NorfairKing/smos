{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Next where

import Data.Maybe


import Path

import Smos.Data

isNextAction :: Entry -> Bool
isNextAction entry =
    or $
    (==) (entryState entry) . Just <$> mapMaybe todoState ["NEXT", "STARTED"]

makeNextActionEntry :: Path Rel File -> Entry -> NextActionEntry
makeNextActionEntry rf e =
    NextActionEntry
        { nextActionEntryTodoState = entryState e
        , nextActionEntryHeader = entryHeader e
        , nextActionEntryFilePath = rf
        }

data NextActionEntry = NextActionEntry
    { nextActionEntryTodoState :: Maybe TodoState
    , nextActionEntryHeader :: Header
    , nextActionEntryFilePath :: Path Rel File
    } deriving (Show, Eq)
