{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler
  ( smosScheduler
  ) where

import GHC.Generics (Generic)

import qualified Data.ByteString as SB
import Data.Char as Char
import Data.Function
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Yaml as Yaml
import Text.Show.Pretty

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
          SB.putStrLn contents
          now <- getCurrentTime
          let ctx = RenderContext {renderContextTime = now}
          let vRendered = runReaderT (renderTemplate template) ctx
          case vRendered of
            Failure errs ->
              putStrLn $
              unlines $
              "WARNING: Validation errors while rendering template:" : map prettyRenderError errs
            Success rendered -> do
              destinationExists <- doesFileExist to
              when destinationExists $
                putStrLn $
                unwords ["WARNING: destination already exists:", fromAbsFile to, "overwriting."]
              ensureDir $ parent to
              writeSmosFile to rendered
              cs <- SB.readFile $ fromAbsFile to
              SB.putStrLn cs

renderTemplate :: ScheduleTemplate -> Render SmosFile
renderTemplate (ScheduleTemplate f) = fmap SmosFile $ traverse (traverse renderEntryTemplate) f

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
    Just h -> pure h

renderContentsTemplate :: Maybe Contents -> Render (Maybe Contents)
renderContentsTemplate =
  mapM $ \cs -> do
    t <- renderTextTemplate (contentsText cs)
    case contents t of
      Nothing -> lift $ Failure [RenderErrorContentsValidity cs t]
      Just c -> pure c

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
  asks renderContextTime <&> (\now -> T.pack $ formatTime defaultTimeLocale (T.unpack t) now)

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
  = RenderErrorHeaderValidity Header Text
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
