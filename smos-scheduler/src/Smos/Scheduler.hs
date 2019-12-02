{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler
  ( smosScheduler
  ) where

import GHC.Generics (Generic)

import qualified Data.ByteString as SB
import Data.Char as Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Text as T
import Data.Time
import Data.Yaml as Yaml
import Text.Show.Pretty

import Path
import Path.IO

import Smos.Data

import qualified Smos.Report.Config as Report

import Smos.Scheduler.OptParse
import Smos.Scheduler.OptParse.Types

smosScheduler :: IO ()
smosScheduler = getSettings >>= scheduler

scheduler :: Settings -> IO ()
scheduler sets@Settings {..} = do
  pPrint sets
  wd <- Report.resolveReportWorkflowDir setReportSettings
  mapM_ (handleScheduleItem wd) $ scheduleItems setSchedule

handleScheduleItem :: Path Abs Dir -> ScheduleItem -> IO ()
handleScheduleItem wdir ScheduleItem {..} = do
  let from = wdir </> scheduleItemTemplate
  let to = wdir </> scheduleItemDestination
  pPrint (from, to)
  mContents <- forgivingAbsence $ SB.readFile $ fromAbsFile from
  case mContents of
    Nothing -> putStrLn $ unwords ["WARNING: template does not exist:", fromAbsFile from]
    Just contents ->
      case Yaml.decodeEither' contents of
        Left err ->
          putStrLn $
          unlines
            [ unwords ["WARNING: Does not look like a smos template file:", fromAbsFile from]
            , prettyPrintParseException err
            ]
        Right template -> do
          pPrint template
          let rendered = renderTemplate template
          pPrint rendered
          destinationExists <- doesFileExist to
          if destinationExists
            then putStrLn $
                 unwords
                   ["WARNING: destination already exists:", fromAbsFile to, "not overwriting."]
            else do
              ensureDir $ parent to
              writeSmosFile to rendered

renderTemplate :: ScheduleTemplate -> SmosFile
renderTemplate (ScheduleTemplate f) = SmosFile $ map (fmap renderEntryTemplate) f

renderEntryTemplate :: EntryTemplate -> Entry
renderEntryTemplate EntryTemplate {..} =
  Entry
    { entryHeader = renderHeaderTemplate entryTemplateHeader
    , entryContents = fmap renderContentsTemplate entryTemplateContents
    , entryTimestamps = renderTimestampsTemplate entryTemplateTimestamps
    , entryProperties = renderPropertiesTemplate entryTemplateProperties
    , entryStateHistory = renderStateHistoryTemplate entryTemplateStateHistory
    , entryTags = renderTagsTemplate entryTemplateTags
    , entryLogbook = emptyLogbook
    }

renderHeaderTemplate :: Header -> Header
renderHeaderTemplate = id -- TODO

renderContentsTemplate :: Contents -> Contents
renderContentsTemplate = id -- TODO

renderTimestampsTemplate :: Map TimestampName TimestampTemplate -> Map TimestampName Timestamp
renderTimestampsTemplate = M.map renderTimestampTemplate -- TODO

renderTimestampTemplate :: TimestampTemplate -> Timestamp
renderTimestampTemplate = undefined

renderPropertiesTemplate :: Map PropertyName PropertyValue -> Map PropertyName PropertyValue
renderPropertiesTemplate = id -- TODO

renderStateHistoryTemplate :: StateHistoryTemplate -> StateHistory
renderStateHistoryTemplate (StateHistoryTemplate es) =
  StateHistory $ map renderStateHistoryEntryTemplate es

renderStateHistoryEntryTemplate
          :: StateHistoryEntryTemplate -> StateHistoryEntry
renderStateHistoryEntryTemplate = undefined


renderTagsTemplate :: Set Tag -> Set Tag
renderTagsTemplate = id -- TODO
