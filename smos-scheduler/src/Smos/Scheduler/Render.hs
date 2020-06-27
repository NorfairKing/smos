{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler.Render where

import Control.Monad
import Control.Monad.Reader
import qualified Data.Aeson as JSON
import Data.FuzzyTime
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Traversable
import Data.Validity
import GHC.Generics (Generic)
import Path
import Smos.Data
import Smos.Scheduler.OptParse
import Smos.Scheduler.Template
import Text.Megaparsec

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
renderTimestampsTemplate = traverse renderTimestampTemplate

renderTimestampTemplate :: TimestampTemplate -> Render Timestamp
renderTimestampTemplate (TimestampTemplate t) = do
  rt <- renderTextTemplate t
  case JSON.eitherDecode (JSON.encode rt) of
    Left err -> lift $ Failure [RenderErrorTimestampParseError t rt err]
    Right ts -> pure ts

renderPropertiesTemplate ::
  Map PropertyName PropertyValue -> Render (Map PropertyName PropertyValue)
renderPropertiesTemplate = traverse renderPropertyValueTemplate

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
renderStateHistoryEntryTemplate StateHistoryEntryTemplate {..} = do
  stateHistoryEntryNewState <- mapM renderTodoStateTemplate stateHistoryEntryTemplateNewState
  stateHistoryEntryTimestamp <- renderUTCTimeTemplate stateHistoryEntryTemplateTimestamp
  pure StateHistoryEntry {..}

renderTodoStateTemplate :: TodoState -> Render TodoState
renderTodoStateTemplate = fmap TodoState . renderTextTemplate . todoStateText

renderUTCTimeTemplate :: UTCTimeTemplate -> Render UTCTime
renderUTCTimeTemplate (UTCTimeTemplate t) = do
  rt <- renderTextTemplate t
  case JSON.eitherDecode (JSON.encode rt) of
    Left err -> lift $ Failure [RenderErrorUTCTimeParseError t rt err]
    Right ts -> pure ts

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
  tz <- asks renderContextTimeZone
  fmap T.concat $ forM tps $ \case
    TLit t -> pure t
    TTime t -> pure $ T.pack $ formatTime defaultTimeLocale (T.unpack t) now
    TRelTime tt rtt -> case parse fuzzyLocalTimeP (show rtt) rtt of
      Left err -> lift $ Failure [RenderErrorRelativeTimeParserError rtt (errorBundlePretty err)]
      Right flt ->
        pure $ T.pack $ case resolveLocalTime (utcToLocalTime tz now) flt of
          OnlyDaySpecified d -> formatTime defaultTimeLocale (T.unpack tt) d
          BothTimeAndDay lt -> formatTime defaultTimeLocale (T.unpack tt) lt

type Render a = ReaderT RenderContext RenderValidation a

data RenderValidation a
  = Success a
  | Failure [RenderError]
  deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (RenderValidation a)

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
  | RenderErrorUTCTimeParseError Text Text String
  | RenderErrorTimestampParseError Text Text String
  | RenderErrorEntrySetState Entry UTCTime
  | RenderErrorTemplateParseError Text String
  | RenderErrorRelativeTimeParserError Text String
  deriving (Show, Eq, Generic)

instance Validity RenderError

prettyRenderError :: RenderError -> String
prettyRenderError = show

data RenderContext
  = RenderContext
      { renderContextTime :: UTCTime,
        renderContextTimeZone :: TimeZone
      }
  deriving (Show, Generic)

instance Validity RenderContext
