{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Single
  ( smosSingle
  ) where

import Data.Char as Char
import qualified Data.Text as T
import Data.Time
import Path

import Smos.Data

import Smos.Report.Config

import Smos.Single.OptParse
import Smos.Single.OptParse.Types

smosSingle :: IO ()
smosSingle = getSettings >>= archive

archive :: Settings -> IO ()
archive Settings {..} = do
  wd <- resolveReportWorkflowDir setReportSettings
  path <-
    (wd </>) <$>
    case setTaskFile of
      Just rp -> pure rp
      Nothing -> deriveFileName setTask
  now <- getCurrentTime
  let smosFile = makeSingleSmosFile now setTask
  writeSmosFile path smosFile

deriveFileName :: Header -> IO (Path Rel File)
deriveFileName h = parseRelFile $ addExtension $ map go $ T.unpack $ headerText h
  where
    addExtension = (++ ".smos")
    go :: Char -> Char
    go ' ' = '-'
    go c = Char.toLower c

makeSingleSmosFile :: UTCTime -> Header -> SmosFile
makeSingleSmosFile now h = SmosFile {smosFileForest = [Node e []]}
  where
    e :: Entry
    e =
      (newEntry h)
        { entryStateHistory =
            StateHistory
              [ StateHistoryEntry
                  { stateHistoryEntryTimestamp = now
                  , stateHistoryEntryNewState = Just $ TodoState "NEXT"
                  }
              ]
        }
