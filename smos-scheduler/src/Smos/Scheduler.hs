{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler
  ( smosScheduler,
  )
where

import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Traversable
import Data.Yaml as Yaml
import GHC.Generics (Generic)
import Path
import Path.IO
import Smos.Data
import qualified Smos.Report.Config as Report
import Smos.Scheduler.OptParse
import Smos.Scheduler.Template
import System.Cron (nextMatch, scheduleMatches)
import System.Exit
import Text.Show.Pretty

smosScheduler :: IO ()
smosScheduler = getSettings >>= scheduler

scheduler :: Settings -> IO ()
scheduler Settings {..} = do
  wd <- Report.resolveDirWorkflowDir setDirectorySettings
  mContents <- forgivingAbsence $ SB.readFile $ fromAbsFile setStateFile
  mState <-
    case mContents of
      Nothing -> pure Nothing
      Just cts ->
        case Yaml.decodeEither' cts of
          Left err ->
            die $
              unlines
                [ unwords ["ERROR: unable to decode state file:", fromAbsFile setStateFile],
                  prettyPrintParseException err
                ]
          Right state -> pure $ Just state
  now <- getCurrentTime
  let goAhead =
        case mState of
          Nothing -> True
          Just ScheduleState {..} -> diffUTCTime now scheduleStateLastRun >= minimumScheduleInterval
  if goAhead
    then do
      mapM_ (handleScheduleItem mState wd now) $ scheduleItems setSchedule
      let state' =
            case mState of
              Nothing -> ScheduleState {scheduleStateLastRun = now}
              Just state -> state {scheduleStateLastRun = now}
      SB.writeFile (fromAbsFile setStateFile) (Yaml.encode state')
    else putStrLn "Not running because it's been run too recently already."

minimumScheduleInterval :: NominalDiffTime
minimumScheduleInterval = 60 -- Only run once per minute.

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
    Nothing -> putStrLn $ unwords ["Not activating", show s, "at current time", show now]
    Just scheduledTime -> performScheduleItem wdir scheduledTime se

performScheduleItem :: Path Abs Dir -> UTCTime -> ScheduleItem -> IO ()
performScheduleItem wdir now ScheduleItem {..} = do
  let from = wdir </> scheduleItemTemplate
  let ctx = RenderContext {renderContextTime = now}
  case runReaderT (renderPathTemplate scheduleItemDestination) ctx of
    Failure errs ->
      putStrLn
        $ unlines
        $ "ERROR: Validation errors while rendering template destination file name:"
          : map prettyRenderError errs
    Success destination -> do
      let to = wdir </> destination
      pPrint from
      pPrint to
      mContents <- forgivingAbsence $ SB.readFile $ fromAbsFile from
      case mContents of
        Nothing -> putStrLn $ unwords ["ERROR: template does not exist:", fromAbsFile from]
        Just cts ->
          case Yaml.decodeEither' cts of
            Left err ->
              putStrLn $
                unlines
                  [ unwords ["ERROR: Does not look like a smos template file:", fromAbsFile from],
                    prettyPrintParseException err
                  ]
            Right template -> do
              SB8.putStrLn cts
              let vRendered = runReaderT (renderTemplate template) ctx
              case vRendered of
                Failure errs ->
                  putStrLn
                    $ unlines
                    $ "ERROR: Validation errors while rendering template:"
                      : map prettyRenderError errs
                Success rendered -> do
                  destinationExists <- doesFileExist to
                  when destinationExists
                    $ putStrLn
                    $ unwords ["WARNING: destination already exists:", fromAbsFile to, "overwriting."]
                  ensureDir $ parent to
                  writeSmosFile to rendered
                  cs <- SB.readFile $ fromAbsFile to
                  SB8.putStrLn cs

renderTemplate :: ScheduleTemplate -> Render SmosFile
renderTemplate (ScheduleTemplate f) = do
  now <- asks renderContextTime
  renderedForest <- traverse (traverse renderEntryTemplate) f
  fmap SmosFile
    $ for renderedForest
    $ \tree ->
      for tree $ \entry ->
        case entrySetState now (Just "TODO") entry of
          Nothing -> lift $ Failure [RenderErrorEntrySetState entry now]
          Just r -> pure r

renderEntryTemplate :: EntryTemplate -> Render Entry
renderEntryTemplate EntryTemplate {..} =
  Entry <$> renderHeaderTemplate entryTemplateHeader
    <*> renderContentsTemplate entryTemplateContents
    <*> renderTimestampsTemplate entryTemplateTimestamps
    <*> renderPropertiesTemplate entryTemplateProperties
    <*> renderStateHistoryTemplate entryTemplateStateHistory
    <*> renderTagsTemplate entryTemplateTags
    <*> pure emptyLogbook

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
renderTextTemplate t =
  case parseTimeTemplate t of
    Left err -> lift $ Failure [RenderErrorTemplateParseError t err]
    Right templ -> renderTimeTemplateNow templ

renderPathTemplate :: Path Rel File -> Render (Path Rel File)
renderPathTemplate rf = do
  let s = fromRelFile rf
  case parseTimeTemplate (T.pack s) of
    Left err -> lift $ Failure [RenderErrorTemplateParseError (T.pack s) err]
    Right templ -> do
      t' <- renderTimeTemplateNow templ
      case parseRelFile (T.unpack t') of
        Nothing -> lift $ Failure [RenderErrorPathValidity rf s]
        Just rf' -> pure rf'

renderTimeTemplateNow :: Template -> Render Text
renderTimeTemplateNow (Template tps) = do
  now <- asks renderContextTime
  fmap T.concat $ forM tps $ \case
    TLit t -> pure t
    TTime t -> pure $ T.pack $ formatTime defaultTimeLocale (T.unpack t) now

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
  | RenderErrorEntrySetState Entry UTCTime
  | RenderErrorTemplateParseError Text String
  deriving (Show, Eq, Generic)

prettyRenderError :: RenderError -> String
prettyRenderError = show

data RenderContext
  = RenderContext
      { renderContextTime :: UTCTime
      }
  deriving (Show, Eq, Generic)
