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
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Zones
import Data.Validity
import GHC.Generics (Generic)
import Path
import Smos.Data
import Smos.Scheduler.OptParse
import Smos.Scheduler.Template
import Text.Megaparsec

renderTemplate :: ScheduleTemplate -> Render SmosFile
renderTemplate (ScheduleTemplate f) =
  makeSmosFile <$> traverse (traverse renderEntryTemplate) f

renderEntryTemplate :: EntryTemplate -> Render Entry
renderEntryTemplate EntryTemplate {..} =
  Entry
    <$> renderHeaderTemplate entryTemplateHeader
    <*> renderContentsTemplate entryTemplateContents
    <*> renderTimestampsTemplate entryTemplateTimestamps
    <*> renderPropertiesTemplate entryTemplateProperties
    <*> renderStateHistoryTemplate entryTemplateState
    <*> renderTagsTemplate entryTemplateTags
    <*> pure emptyLogbook

renderHeaderTemplate :: Header -> Render Header
renderHeaderTemplate h = do
  t <- renderTextTemplate (headerText h)
  case header t of
    Nothing -> renderFail $ RenderErrorHeaderValidity h t
    Just h' -> pure h'

renderContentsTemplate :: Maybe Contents -> Render (Maybe Contents)
renderContentsTemplate =
  mapM $ \cs -> do
    t <- renderTextTemplate (contentsText cs)
    case contents t of
      Nothing -> renderFail $ RenderErrorContentsValidity cs t
      Just cs' -> pure cs'

renderTimestampsTemplate ::
  Map TimestampName TimestampTemplate -> Render (Map TimestampName Timestamp)
renderTimestampsTemplate = traverse renderTimestampTemplate

renderTimestampTemplate :: TimestampTemplate -> Render Timestamp
renderTimestampTemplate (TimestampTemplate t) = do
  rt <- renderTextTemplate t
  case JSON.eitherDecode (JSON.encode rt) of
    Left err -> renderFail $ RenderErrorTimestampParseError t rt err
    Right ts -> pure ts

renderPropertiesTemplate ::
  Map PropertyName PropertyValue -> Render (Map PropertyName PropertyValue)
renderPropertiesTemplate = traverse renderPropertyValueTemplate

renderPropertyValueTemplate :: PropertyValue -> Render PropertyValue
renderPropertyValueTemplate pv = do
  t <- renderTextTemplate (propertyValueText pv)
  case propertyValue t of
    Nothing -> renderFail $ RenderErrorPropertyValueValidity pv t
    Just pv' -> pure pv'

-- | `Nothing` means the state field doesn't exist, while `Just Nothing`
-- means it is explicitly set to `null`.
renderStateHistoryTemplate :: Maybe (Maybe TodoState) -> Render StateHistory
renderStateHistoryTemplate mts = do
  now <- asks renderContextNow
  pure $
    StateHistory
      [ mkStateHistoryEntry now (fromMaybe (Just "TODO") mts)
      ]

renderTodoStateTemplate :: TodoState -> Render TodoState
renderTodoStateTemplate = fmap TodoState . renderTextTemplate . todoStateText

renderUTCTimeTemplate :: UTCTimeTemplate -> Render UTCTime
renderUTCTimeTemplate (UTCTimeTemplate t) = do
  rt <- renderTextTemplate t
  case JSON.eitherDecode (JSON.encode rt) of
    Left err -> renderFail $ RenderErrorUTCTimeParseError t rt err
    Right ts -> pure ts

renderTagsTemplate :: Set Tag -> Render (Set Tag)
renderTagsTemplate = fmap S.fromList . mapM renderTagTemplate . S.toList

renderTagTemplate :: Tag -> Render Tag
renderTagTemplate tg = do
  t <- renderTextTemplate (tagText tg)
  case tag t of
    Nothing -> renderFail $ RenderErrorTagValidity tg t
    Just tg' -> pure tg'

renderTextTemplate :: Text -> Render Text
renderTextTemplate t =
  case parseTimeTemplate t of
    Left err -> renderFail $ RenderErrorTemplateParseError t err
    Right templ -> renderTimeTemplateNow templ

renderDestinationPathTemplate :: DestinationPathTemplate -> Render (Path Rel File)
renderDestinationPathTemplate (DestinationPathTemplate rf) = renderPathTemplate rf

renderPathTemplate :: Path Rel File -> Render (Path Rel File)
renderPathTemplate rf = do
  let s = fromRelFile rf
  case parseTimeTemplate (T.pack s) of
    Left err -> renderFail $ RenderErrorTemplateParseError (T.pack s) err
    Right templ -> do
      t' <- renderTimeTemplateNow templ
      case parseRelFile (T.unpack t') of
        Nothing -> renderFail $ RenderErrorPathValidity rf s
        Just rf' -> pure rf'

renderTimeTemplateNow :: Template -> Render Text
renderTimeTemplateNow (Template tps) = do
  pretendTime <- asks renderContextPretendTime
  fmap T.concat $
    forM tps $ \case
      TLit t -> pure t
      TTime t -> pure $ T.pack $ formatTime defaultTimeLocale (T.unpack t) pretendTime
      TRelTime tt rtt -> case parse fuzzyLocalTimeP (show rtt) rtt of
        Left err -> renderFail $ RenderErrorRelativeTimeParserError rtt (errorBundlePretty err)
        Right flt ->
          pure $
            T.pack $ case fromMaybe (BothTimeAndDay pretendTime) (resolveLocalTimeForwards pretendTime flt) of
              OnlyDaySpecified d -> formatTime defaultTimeLocale (T.unpack tt) d
              BothTimeAndDay lt -> formatTime defaultTimeLocale (T.unpack tt) lt

type Render a = ReaderT RenderContext RenderValidation a

runRenderAsIfAt :: LocalTime -> Render a -> IO (Either (NonEmpty RenderError) a)
runRenderAsIfAt pretendTime func = do
  now <- getCurrentTime
  zone <- loadLocalTZ
  let ctx =
        RenderContext
          { renderContextNow = now,
            renderContextPretendTime = pretendTime,
            renderContextTimeZone = zone
          }
  pure $ runRenderRaw ctx func

runRenderNow :: Render a -> IO (Either (NonEmpty RenderError) a)
runRenderNow func = do
  now <- getCurrentTime
  zone <- loadLocalTZ
  let ctx =
        RenderContext
          { renderContextNow = now,
            renderContextPretendTime = utcToLocalTimeTZ zone now,
            renderContextTimeZone = zone
          }
  pure $ runRenderRaw ctx func

-- You probably don't want to use this, but 'runRenderNow' or 'runRenderAsIfAt' instead.
runRenderRaw :: RenderContext -> Render a -> Either (NonEmpty RenderError) a
runRenderRaw ctx func = case runReaderT func ctx of
  Success a -> Right a
  Failure errs -> Left errs

data RenderValidation a
  = Success a
  | Failure (NonEmpty RenderError)
  deriving (Show, Eq, Generic, Functor)

instance (Validity a) => Validity (RenderValidation a)

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

renderFail :: RenderError -> Render a
renderFail e = lift $ Failure (e :| [])

prettyRenderError :: RenderError -> String
prettyRenderError = show

data RenderContext = RenderContext
  { renderContextNow :: !UTCTime,
    renderContextPretendTime :: !LocalTime,
    renderContextTimeZone :: !TZ
  }
  deriving (Show, Generic)

instance Validity RenderContext
