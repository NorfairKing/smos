{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Data.Types
    ( SmosFile(..)
    , Forest
    , Tree(..)
    , Entry(..)
    , newEntry
    , emptyEntry
    , TodoState(..)
    , Header
    , headerText
    , emptyHeader
    , header
    , Contents(..)
    , emptyContents
    , nullContents
    , PropertyName
    , propertyNameText
    , emptyPropertyName
    , propertyName
    , PropertyValue
    , propertyValueText
    , emptyPropertyValue
    , propertyValue
    , StateHistory(..)
    , StateHistoryEntry(..)
    , emptyStateHistory
    , nullStateHistory
    , Tag
    , tagText
    , emptyTag
    , tag
    , Logbook(..)
    , emptyLogbook
    , nullLogbook
    , logbookClockIn
    , logbookClockOut
    , LogbookEntry(..)
    , TimestampName
    , timestampNameText
    , timestampName
    , emptyTimestampName
    , Timestamp(..)
    , timestampDayFormat
    -- Utils
    , ForYaml(..)
    ) where

import GHC.Generics (Generic)

import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Text ()
import Data.Validity.Time ()

import Data.Aeson as JSON
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Tree
import Data.Yaml.Builder (ToYaml(..))
import qualified Data.Yaml.Builder as Yaml

import Control.Applicative
import Control.Arrow

newtype SmosFile = SmosFile
    { smosFileForest :: Forest Entry
    } deriving (Show, Eq, Generic)

instance Validity SmosFile

instance FromJSON SmosFile where
    parseJSON v = (SmosFile . unForYaml) <$> parseJSON v

instance ToJSON SmosFile where
    toJSON = toJSON . ForYaml . smosFileForest

instance ToYaml SmosFile where
    toYaml = toYaml . ForYaml . smosFileForest

newtype ForYaml a = ForYaml
    { unForYaml :: a
    } deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (ForYaml a)

instance FromJSON (ForYaml (Forest Entry)) where
    parseJSON v = do
        els <- parseJSON v
        ts <- mapM (fmap unForYaml . parseJSON) els
        pure $ ForYaml ts

instance ToJSON (ForYaml (Forest Entry)) where
    toJSON = toJSON . map ForYaml . unForYaml

instance ToYaml (ForYaml (Forest Entry)) where
    toYaml = toYaml . map ForYaml . unForYaml

instance FromJSON (ForYaml (Tree Entry)) where
    parseJSON v =
        ForYaml <$>
        (((withObject "Tree Entry" $ \o ->
               Node <$> o .: "entry" <*>
               (unForYaml <$> o .:? "forest" .!= ForYaml []))
              v) <|>
         (Node <$> parseJSON v <*> pure []))

instance ToJSON (ForYaml (Tree Entry)) where
    toJSON (ForYaml Node {..}) =
        if null subForest
            then toJSON rootLabel
            else object $
                 [("entry" .= rootLabel), ("forest" .= ForYaml subForest)]

instance ToYaml (ForYaml (Tree Entry)) where
    toYaml (ForYaml Node {..}) =
        if null subForest
            then toYaml rootLabel
            else Yaml.mapping $
                 [ ("entry", toYaml rootLabel)
                 , ("forest", toYaml (ForYaml subForest))
                 ]

data Entry = Entry
    { entryHeader :: Header
    , entryContents :: Maybe Contents
    , entryTimestamps :: Map TimestampName Timestamp -- SCHEDULED, DEADLINE, etc.
    , entryProperties :: Map PropertyName PropertyValue
    , entryStateHistory :: StateHistory -- TODO, DONE, etc.
    , entryTags :: [Tag] -- '@home', 'toast', etc.
    , entryLogbook :: Logbook
    } deriving (Show, Eq, Ord, Generic)

newEntry :: Header -> Entry
newEntry h =
    Entry
        { entryHeader = h
        , entryContents = Nothing
        , entryTimestamps = M.empty
        , entryProperties = M.empty
        , entryStateHistory = emptyStateHistory
        , entryTags = []
        , entryLogbook = emptyLogbook
        }

emptyEntry :: Entry
emptyEntry = newEntry emptyHeader

instance Validity Entry

instance FromJSON Entry where
    parseJSON v =
        (do h <- parseJSON v
            pure $ newEntry h) <|>
        (withObject "Entry" $ \o ->
             Entry <$> o .:? "header" .!= emptyHeader <*> o .:? "contents" <*>
             o .:? "timestamps" .!= M.empty <*>
             o .:? "properties" .!= M.empty <*>
             o .:? "state-history" .!= StateHistory [] <*>
             o .:? "tags" .!= [] <*>
             o .:? "logbook" .!= emptyLogbook)
            v

instance ToJSON Entry where
    toJSON Entry {..} =
        if and [ isNothing entryContents
               , M.null entryTimestamps
               , M.null entryProperties
               , null $ unStateHistory entryStateHistory
               , null entryTags
               , entryLogbook == emptyLogbook
               ]
            then toJSON entryHeader
            else object $
                 ["header" .= entryHeader] ++
                 ["contents" .= entryContents | isJust entryContents] ++
                 [ "timestamps" .= entryTimestamps
                 | not $ M.null entryTimestamps
                 ] ++
                 [ "properties" .= entryProperties
                 | not $ M.null entryProperties
                 ] ++
                 [ "state-history" .= entryStateHistory
                 | not $ null $ unStateHistory entryStateHistory
                 ] ++
                 ["tags" .= entryTags | not $ null entryTags] ++
                 ["logbook" .= entryLogbook | entryLogbook /= emptyLogbook]

instance ToYaml Entry where
    toYaml Entry {..} =
        if and [ isNothing entryContents
               , M.null entryTimestamps
               , M.null entryProperties
               , null $ unStateHistory entryStateHistory
               , null entryTags
               , entryLogbook == emptyLogbook
               ]
            then toYaml entryHeader
            else Yaml.mapping $
                 [("header", toYaml entryHeader)] ++
                 [("contents", toYaml entryContents) | isJust entryContents] ++
                 [ ( "timestamps"
                   , toYaml $ M.mapKeys timestampNameText entryTimestamps)
                 | not $ M.null entryTimestamps
                 ] ++
                 [ ( "properties"
                   , toYaml $ M.mapKeys propertyNameText entryProperties)
                 | not $ M.null entryProperties
                 ] ++
                 [ ("state-history", toYaml entryStateHistory)
                 | not $ null $ unStateHistory entryStateHistory
                 ] ++
                 [("tags", toYaml entryTags) | not $ null entryTags] ++
                 [ ("logbook", toYaml entryLogbook)
                 | entryLogbook /= emptyLogbook
                 ]

newtype Header = Header
    { headerText :: Text
    } deriving (Show, Eq, Ord, Generic, IsString, ToJSON, ToYaml)

instance Validity Header where
    validate (Header t) =
        mconcat
            [ delve "headerText" t
            , decorateList (T.unpack t) $ \c ->
                  declare "The character is not a newline character" $ c /= '\n'
            ]

instance FromJSON Header where
    parseJSON =
        withText "Header" $ \t ->
            case header t of
                Nothing -> fail $ "Invalid header: " <> T.unpack t
                Just h -> pure h

emptyHeader :: Header
emptyHeader = Header ""

header :: Text -> Maybe Header
header = constructValid . Header

newtype Contents = Contents
    { contentsText :: Text
    } deriving (Show, Eq, Ord, Generic, IsString, FromJSON, ToJSON, ToYaml)

instance Validity Contents

emptyContents :: Contents
emptyContents = Contents ""

nullContents :: Contents -> Bool
nullContents = (== emptyContents)

newtype PropertyName = PropertyName
    { propertyNameText :: Text
    } deriving (Show, Eq, Ord, Generic, IsString, ToJSON, ToJSONKey, ToYaml)

instance Validity PropertyName where
    validate (PropertyName t) =
        mconcat
            [ delve "propertyNameText" t
            , decorateList (T.unpack t) $ \c ->
                  declare "The character is not a newline character" $ c /= '\n'
            ]

instance FromJSON PropertyName where
    parseJSON = withText "PropertyName" parsePropertyName

instance FromJSONKey PropertyName where
    fromJSONKey = FromJSONKeyTextParser parsePropertyName

parsePropertyName :: Monad m => Text -> m PropertyName
parsePropertyName t =
    case propertyName t of
        Nothing -> fail $ "Invalid property name: " <> T.unpack t
        Just h -> pure h

emptyPropertyName :: PropertyName
emptyPropertyName = PropertyName ""

propertyName :: Text -> Maybe PropertyName
propertyName = constructValid . PropertyName

newtype PropertyValue = PropertyValue
    { propertyValueText :: Text
    } deriving (Show, Eq, Ord, Generic, IsString, ToJSON, ToJSONKey, ToYaml)

instance Validity PropertyValue

instance FromJSON PropertyValue where
    parseJSON = withText "PropertyValue" parsePropertyValue

instance FromJSONKey PropertyValue where
    fromJSONKey = FromJSONKeyTextParser parsePropertyValue
parsePropertyValue :: Monad m => Text -> m PropertyValue
parsePropertyValue t =
    case propertyValue t of
        Nothing -> fail $ "Invalid property value: " <> T.unpack t
        Just h -> pure h

emptyPropertyValue :: PropertyValue
emptyPropertyValue = PropertyValue ""

propertyValue :: Text -> Maybe PropertyValue
propertyValue = constructValid . PropertyValue

newtype TimestampName = TimestampName
    { timestampNameText :: Text
    } deriving (Show, Eq, Ord, Generic, IsString, ToJSON, ToJSONKey, ToYaml)

instance Validity TimestampName where
    validate (TimestampName t) =
        mconcat
            [ delve "timestampNameText" t
            , decorateList (T.unpack t) $ \c ->
                  declare "The character is not a newline character" $ c /= '\n'
            ]

instance FromJSON TimestampName where
    parseJSON = withText "TimestampName" parseTimestampName

instance FromJSONKey TimestampName where
    fromJSONKey = FromJSONKeyTextParser parseTimestampName
parseTimestampName :: Monad m => Text -> m TimestampName
parseTimestampName t =
    case timestampName t of
        Nothing -> fail $ "Invalid timestamp name: " <> T.unpack t
        Just h -> pure h

emptyTimestampName :: TimestampName
emptyTimestampName = TimestampName ""

timestampName :: Text -> Maybe TimestampName
timestampName = constructValid . TimestampName

data Timestamp = Timestamp
    { timestampDay :: Day
    } deriving (Show, Eq, Ord, Generic)

instance Validity Timestamp

instance FromJSON Timestamp where
    parseJSON v = do
        s <- parseJSON v
        Timestamp <$> parseTimeM False defaultTimeLocale timestampDayFormat s

instance ToJSON Timestamp where
    toJSON (Timestamp d) =
        toJSON $ T.pack $ formatTime defaultTimeLocale timestampDayFormat d

instance ToYaml Timestamp where
    toYaml =
        toYaml .
        T.pack . formatTime defaultTimeLocale timestampDayFormat . timestampDay

timestampDayFormat :: String
timestampDayFormat = "%F"

newtype TodoState = TodoState
    { todoStateText :: Text
    } deriving (Show, Eq, Ord, Generic, IsString, FromJSON, ToJSON, ToYaml)

instance Validity TodoState

newtype StateHistory = StateHistory
    { unStateHistory :: [StateHistoryEntry]
    } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, ToYaml)

instance Validity StateHistory where
    validate st@(StateHistory hs) =
        genericValidate st <>
        declare
            "The entries are stored in reverse chronological order"
            (hs <= sort hs)

emptyStateHistory :: StateHistory
emptyStateHistory = StateHistory []

nullStateHistory :: StateHistory -> Bool
nullStateHistory = (== emptyStateHistory)

data StateHistoryEntry = StateHistoryEntry
    { stateHistoryEntryNewState :: Maybe TodoState
    , stateHistoryEntryTimestamp :: UTCTime
    } deriving (Show, Eq, Generic)

instance Ord StateHistoryEntry where
    compare =
        mconcat
            [ comparing $ Down . stateHistoryEntryTimestamp
            , comparing stateHistoryEntryNewState
            ]

instance Validity StateHistoryEntry

instance FromJSON StateHistoryEntry where
    parseJSON =
        withObject "StateHistoryEntry" $ \o ->
            StateHistoryEntry <$> o .: "new-state" <*> o .: "timestamp"

instance ToJSON StateHistoryEntry where
    toJSON StateHistoryEntry {..} =
        object
            [ "new-state" .= stateHistoryEntryNewState
            , "timestamp" .= stateHistoryEntryTimestamp
            ]

instance ToYaml StateHistoryEntry where
    toYaml StateHistoryEntry {..} =
        Yaml.mapping
            [ ("new-state", toYaml stateHistoryEntryNewState)
            , ("timestamp", toYaml stateHistoryEntryTimestamp)
            ]

newtype Tag = Tag
    { tagText :: Text
    } deriving (Show, Eq, Ord, Generic, IsString, ToJSON, ToYaml)

instance Validity Tag where
    validate (Tag t) =
        mconcat
            [ delve "tagText" t
            , decorateList (T.unpack t) $ \c ->
                  declare "The character is not a newline character" $ c /= '\n'
            ]

instance FromJSON Tag where
    parseJSON =
        withText "Tag" $ \t ->
            case tag t of
                Nothing -> fail $ "Invalid tag: " <> T.unpack t
                Just h -> pure h

emptyTag :: Tag
emptyTag = Tag ""

tag :: Text -> Maybe Tag
tag = constructValid . Tag

data Logbook
    = LogOpen UTCTime
              [LogbookEntry]
    | LogClosed [LogbookEntry]
    deriving (Show, Eq, Ord, Generic)

instance Validity Logbook where
    validate lo@(LogOpen utct lbes) =
        mconcat
            [ genericValidate lo
            , declare "The open time occurred after the last entry ended" $
              case lbes of
                  [] -> True
                  (lbe:_) -> utct >= logbookEntryEnd lbe
            , decorate "The consecutive logbook entries happen after each other" $
              decorateList (conseqs lbes) $ \(lbe1, lbe2) ->
                  declare "The former happens after the latter" $
                  logbookEntryStart lbe1 >= logbookEntryEnd lbe2
            ]
    validate lc@(LogClosed lbes) =
        mconcat
            [ genericValidate lc
            , decorate "The consecutive logbook entries happen after each other" $
              decorateList (conseqs lbes) $ \(lbe1, lbe2) ->
                  declare "The former happens after the latter" $
                  logbookEntryStart lbe1 >= logbookEntryEnd lbe2
            ]

conseqs :: [a] -> [(a, a)]
conseqs [] = []
conseqs [_] = []
conseqs (a:b:as) = (a, b) : conseqs (b : as)

instance FromJSON Logbook where
    parseJSON v = do
        els <- parseJSON v
        case els of
            [] -> pure $ LogClosed []
            (e:es) -> do
                (start, mend) <-
                    withObject
                        "First logbook entry"
                        (\o -> (,) <$> o .: "start" <*> o .:? "end")
                        e
                rest <- mapM parseJSON es
                let candidate =
                        case mend of
                            Nothing -> LogOpen start rest
                            Just end ->
                                LogClosed $ LogbookEntry start end : rest
                case prettyValidation candidate of
                    Left err ->
                        fail $
                        unlines ["JSON represented an invalid logbook:", err]
                    Right r -> pure r

instance ToJSON Logbook where
    toJSON = toJSON . go
      where
        go (LogOpen start rest) = object ["start" .= start] : map toJSON rest
        go (LogClosed rest) = map toJSON rest

instance ToYaml Logbook where
    toYaml = toYaml . go
      where
        go (LogOpen start rest) =
            Yaml.mapping [("start", toYaml start)] : map toYaml rest
        go (LogClosed rest) = map toYaml rest

emptyLogbook :: Logbook
emptyLogbook = LogClosed []

nullLogbook :: Logbook -> Bool
nullLogbook = (== emptyLogbook)

logbookClockIn :: UTCTime -> Logbook -> Maybe Logbook
logbookClockIn now lb =
    case lb of
        LogOpen _ _ -> Nothing
        LogClosed ls -> constructValid $ LogOpen now ls

logbookClockOut :: UTCTime -> Logbook -> Maybe Logbook
logbookClockOut now lb =
    case lb of
        LogClosed _ -> Nothing
        LogOpen u ls ->
            constructValid $
            LogClosed $
            LogbookEntry {logbookEntryStart = u, logbookEntryEnd = now} : ls

data LogbookEntry = LogbookEntry
    { logbookEntryStart :: UTCTime
    , logbookEntryEnd :: UTCTime
    } deriving (Show, Eq, Ord, Generic)

instance Validity LogbookEntry where
    validate lbe@LogbookEntry {..} =
        mconcat
            [ genericValidate lbe
            , declare "The start time occurred before the end time" $
              logbookEntryStart <= logbookEntryEnd
            ]

instance FromJSON LogbookEntry where
    parseJSON =
        withObject "LogbookEntry" $ \o -> do
            candidate <- LogbookEntry <$> o .: "start" <*> o .: "end"
            case prettyValidation candidate of
                Left err ->
                    fail $
                    unlines ["JSON represented an invalid logbook entry:", err]
                Right r -> pure r

instance ToJSON LogbookEntry where
    toJSON LogbookEntry {..} =
        object ["start" .= logbookEntryStart, "end" .= logbookEntryEnd]

instance ToYaml LogbookEntry where
    toYaml LogbookEntry {..} =
        Yaml.mapping
            [ ("start", toYaml logbookEntryStart)
            , ("end", toYaml logbookEntryEnd)
            ]

instance ToYaml UTCTime where
    toYaml =
        Yaml.string . T.pack . formatTime defaultTimeLocale "%F %H:%M:%S.%q%z"

instance (ToYaml a) => ToYaml (Maybe a) where
    toYaml Nothing = Yaml.null
    toYaml (Just v) = toYaml v

instance (ToYaml a) => ToYaml (Map Text a) where
    toYaml = Yaml.mapping . map (second toYaml) . M.toList
