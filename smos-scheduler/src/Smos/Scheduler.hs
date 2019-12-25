{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler
  ( smosScheduler
  ) where

import GHC.Generics (Generic)

import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Yaml as Yaml
import Text.Show.Pretty

import System.Exit

import System.Cron (nextMatch, scheduleMatches)

import Control.Monad
import Control.Monad.Reader

import Path
import Path.IO

import Smos.Data

import qualified Smos.Report.Config as Report

import Smos.Scheduler.OptParse
import Smos.Scheduler.OptParse.Types

smosScheduler :: IO ()
smosScheduler = getSettings >>= scheduler

scheduler :: Settings -> IO ()
scheduler Settings {..} = do
  wd <- Report.resolveReportWorkflowDir setReportSettings
  mContents <- forgivingAbsence $ SB.readFile $ fromAbsFile setStateFile
  mState <-
    case mContents of
      Nothing -> pure Nothing
      Just cts ->
        case Yaml.decodeEither' cts of
          Left err ->
            die $
            unlines
              [ unwords ["WARNING: unable to decode state file:", fromAbsFile setStateFile]
              , prettyPrintParseException err
              ]
          Right state -> pure $ Just state
  now <- getCurrentTime
  let goAhead =
        case mState of
          Nothing -> True
          Just ScheduleState {..} -> diffUTCTime now scheduleStateLastRun >= 60
  if goAhead
    then do
      mapM_ (handleScheduleItem mState wd now) $ scheduleItems setSchedule
      let state' =
            case mState of
              Nothing -> ScheduleState {scheduleStateLastRun = now}
              Just state -> state {scheduleStateLastRun = now}
      SB.writeFile (fromAbsFile setStateFile) (Yaml.encode state')
    else putStrLn "Not running because it's been run too recently already."

handleScheduleItem :: Maybe ScheduleState -> Path Abs Dir -> UTCTime -> ScheduleItem -> IO ()
handleScheduleItem mState wdir now se = do
  let s = scheduleItemCronSchedule se
  let mScheduledTime =
        case mState of
          Nothing ->
            if scheduleMatches s now
              then Just now
              else Nothing
          Just ScheduleState {..} ->
            case nextMatch s scheduleStateLastRun of
              Nothing -> Nothing
              Just scheduled ->
                if scheduleStateLastRun <= scheduled && scheduled <= now
                  then Just scheduled
                  else Nothing
  case mScheduledTime of
    Nothing -> putStrLn $ unwords ["Not activating ", show s, "at current time", show now]
    Just scheduledTime -> performScheduleItem wdir scheduledTime se

performScheduleItem :: Path Abs Dir -> UTCTime -> ScheduleItem -> IO ()
performScheduleItem wdir now ScheduleItem {..} = do
  let from = wdir </> scheduleItemTemplate
  let ctx = RenderContext {renderContextTime = now}
  case runReaderT (renderPathTemplate scheduleItemDestination) ctx of
    Failure errs ->
      putStrLn $
      unlines $
      "WARNING: Validation errors while rendering template destination file name:" :
      map prettyRenderError errs
    Success destination -> do
      let to = wdir </> destination
      pPrint from
      pPrint to
      mContents <- forgivingAbsence $ SB.readFile $ fromAbsFile from
      case mContents of
        Nothing -> putStrLn $ unwords ["WARNING: template does not exist:", fromAbsFile from]
        Just cts ->
          case Yaml.decodeEither' cts of
            Left err ->
              putStrLn $
              unlines
                [ unwords ["WARNING: Does not look like a smos template file:", fromAbsFile from]
                , prettyPrintParseException err
                ]
            Right template -> do
              SB8.putStrLn cts
              let vRendered = runReaderT (renderTemplate template) ctx
              case vRendered of
                Failure errs ->
                  putStrLn $
                  unlines $
                  "WARNING: Validation errors while rendering template:" :
                  map prettyRenderError errs
                Success rendered -> do
                  destinationExists <- doesFileExist to
                  when destinationExists $
                    putStrLn $
                    unwords ["WARNING: destination already exists:", fromAbsFile to, "overwriting."]
                  ensureDir $ parent to
                  writeSmosFile to rendered
                  cs <- SB.readFile $ fromAbsFile to
                  SB8.putStrLn cs

renderTemplate :: ScheduleTemplate -> Render SmosFile
renderTemplate (ScheduleTemplate f) = SmosFile <$> traverse (traverse renderEntryTemplate) f

renderEntryTemplate :: EntryTemplate -> Render Entry
renderEntryTemplate EntryTemplate {..} =
  Entry <$> renderHeaderTemplate entryTemplateHeader <*>
  renderContentsTemplate entryTemplateContents <*>
  renderTimestampsTemplate entryTemplateTimestamps <*>
  renderPropertiesTemplate entryTemplateProperties <*>
  renderStateHistoryTemplate entryTemplateStateHistory <*>
  renderTagsTemplate entryTemplateTags <*>
  pure emptyLogbook

renderHeaderTemplate :: Header -> Render Header
renderHeaderTemplate h = do
  t <- renderTextTemplate (headerText h)
  case header t of
    Nothing -> lift $ Failure [RenderErrorHeaderValidity h t]
    Just h' -> pure h'

renderContentsTemplate :: Maybe Contents -> Render (Maybe Contents)
renderContentsTemplate =
  mapM $ \cs -> do
    t <- renderTextTemplate (contentsText cs)
    case contents t of
      Nothing -> lift $ Failure [RenderErrorContentsValidity cs t]
      Just cs' -> pure cs'

renderTimestampsTemplate ::
     Map TimestampName TimestampTemplate -> Render (Map TimestampName Timestamp)
renderTimestampsTemplate = traverse renderTimestampTemplate -- TODO

renderTimestampTemplate :: TimestampTemplate -> Render Timestamp
renderTimestampTemplate = undefined -- TODO

renderPropertiesTemplate ::
     Map PropertyName PropertyValue -> Render (Map PropertyName PropertyValue)
renderPropertiesTemplate = traverse renderPropertyValueTemplate -- TODO

renderPropertyValueTemplate :: PropertyValue -> Render PropertyValue
renderPropertyValueTemplate pv = do
  t <- renderTextTemplate (propertyValueText pv)
  case propertyValue t of
    Nothing -> lift $ Failure [RenderErrorPropertyValueValidity pv t]
    Just pv' -> pure pv'

renderStateHistoryTemplate :: StateHistoryTemplate -> Render StateHistory
renderStateHistoryTemplate =
  fmap StateHistory . mapM renderStateHistoryEntryTemplate . stateHistoryEntryTemplates

renderStateHistoryEntryTemplate :: StateHistoryEntryTemplate -> Render StateHistoryEntry
renderStateHistoryEntryTemplate = undefined

renderTagsTemplate :: Set Tag -> Render (Set Tag)
renderTagsTemplate = fmap S.fromList . mapM renderTagTemplate . S.toList

renderTagTemplate :: Tag -> Render Tag
renderTagTemplate tg = do
  t <- renderTextTemplate (tagText tg)
  case tag t of
    Nothing -> lift $ Failure [RenderErrorTagValidity tg t]
    Just tg' -> pure tg'

renderTextTemplate :: Text -> Render Text
renderTextTemplate t = do
  now <- asks renderContextTime
  pure $ T.pack $ formatTime defaultTimeLocale (T.unpack t) now

renderPathTemplate :: Path Rel File -> Render (Path Rel File)
renderPathTemplate rf = do
  now <- asks renderContextTime
  let s = fromRelFile rf
  let s' = formatTime defaultTimeLocale s now
  case parseRelFile s' of
    Nothing -> lift $ Failure [RenderErrorPathValidity rf s]
    Just rf' -> pure rf'

type Render a = ReaderT RenderContext RenderValidation a

data RenderValidation a
  = Success a
  | Failure [RenderError]
  deriving (Show, Eq, Generic, Functor)

instance Applicative RenderValidation where
  pure = Success
  (Success f) <*> (Success a) = Success (f a)
  (Success _) <*> (Failure errs) = Failure errs
  (Failure errs) <*> (Success _) = Failure errs
  (Failure errs1) <*> (Failure errs2) = Failure (errs1 <> errs2)

instance Monad RenderValidation where
  (Success a) >>= f = f a
  (Failure errs) >>= _ = Failure errs

data RenderError
  = RenderErrorPathValidity (Path Rel File) String
  | RenderErrorHeaderValidity Header Text
  | RenderErrorContentsValidity Contents Text
  | RenderErrorTagValidity Tag Text
  | RenderErrorPropertyValueValidity PropertyValue Text
  deriving (Show, Eq, Generic)

prettyRenderError :: RenderError -> String
prettyRenderError = show

data RenderContext =
  RenderContext
    { renderContextTime :: UTCTime
    }
  deriving (Show, Eq, Generic)
