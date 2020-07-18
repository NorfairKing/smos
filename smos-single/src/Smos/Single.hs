{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Single
  ( smosSingle,
  )
where

import Data.Char as Char
import qualified Data.Text as T
import Data.Time
import Path
import Path.IO
import Smos.Data
import Smos.Report.Config
import Smos.Single.OptParse
import System.Exit

smosSingle :: IO ()
smosSingle = getSettings >>= single

single :: Settings -> IO ()
single Settings {..} = do
  wd <- resolveDirWorkflowDir setDirectorySettings
  path <-
    (wd </>)
      <$> case setTaskFile of
        Just rp -> pure rp
        Nothing -> deriveFileName setTask
  exists <- doesFileExist path
  if exists
    then die $ unwords ["There already exists a file at", fromAbsFile path, "not overwriting"]
    else do
      now <- getCurrentTime
      let smosFile = makeSingleSmosFile now setTask
      writeSmosFile path smosFile

deriveFileName :: Header -> IO (Path Rel File)
deriveFileName h = parseRelFile $ (++ ".smos") $ map go $ T.unpack $ headerText h
  where
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
                  { stateHistoryEntryTimestamp = now,
                    stateHistoryEntryNewState = Just $ TodoState "NEXT"
                  }
              ]
        }
