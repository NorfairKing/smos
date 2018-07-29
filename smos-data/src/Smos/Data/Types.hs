{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Smos.Data.Types
    ( SmosFile(..)
    , Forest
    , Tree(..)
    , Entry(..)
    , newEntry
    , TodoState(..)
    , Header(..)
    , Contents(..)
    , PropertyName(..)
    , PropertyValue(..)
    , StateHistory(..)
    , StateHistoryEntry(..)
    , Tag(..)
    , Logbook(..)
    , emptyLogbook
    , LogbookEntry(..)
    , TimestampName(..)
    , Timestamp(..)
    , timestampDayFormat
    , timestampTimeFormat
    , timestampTimeExactFormat
    -- Utils
    , ForYaml(..)
    ) where

import GHC.Generics (Generic)

import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Text ()
import Data.Validity.Time ()

import Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.String
import Data.Text (Text)
import Data.Time
import Data.Tree

import Control.Applicative

newtype SmosFile = SmosFile
    { smosFileForest :: Forest Entry
    } deriving (Show, Eq, Generic)

instance Validity SmosFile

instance FromJSON SmosFile where
    parseJSON v = (SmosFile . unForYaml) <$> parseJSON v

instance ToJSON SmosFile where
    toJSON = toJSON . ForYaml . smosFileForest

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
    toJSON (ForYaml ts) = toJSON $ map ForYaml ts

instance FromJSON (ForYaml (Tree Entry)) where
    parseJSON v =
        ForYaml <$>
        ((Node <$> parseJSON v <*> pure []) <|>
         (withObject "Tree Entry" $ \o ->
              Node <$> o .: "entry" <*>
              (unForYaml <$> o .:? "forest" .!= ForYaml []))
             v)

instance ToJSON (ForYaml (Tree Entry)) where
    toJSON (ForYaml Node {..}) =
        if null subForest
            then toJSON rootLabel
            else object $
                 ("entry" .= rootLabel) :
                 ["forest" .= ForYaml subForest | not (null subForest)]

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
        , entryStateHistory = StateHistory []
        , entryTags = []
        , entryLogbook = emptyLogbook
        }

instance Validity Entry

instance FromJSON Entry where
    parseJSON v =
        (do h <- parseJSON v
            pure $ newEntry h) <|>
        (withObject "Entry" $ \o ->
             Entry <$> o .: "header" <*> o .:? "contents" <*>
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

newtype Header = Header
    { headerText :: Text
    } deriving (Show, Eq, Ord, Generic, IsString, FromJSON, ToJSON)

instance Validity Header

newtype Contents = Contents
    { contentsText :: Text
    } deriving (Show, Eq, Ord, Generic, IsString, FromJSON, ToJSON)

instance Validity Contents

newtype PropertyName = PropertyName
    { propertyNameText :: Text
    } deriving ( Show
               , Eq
               , Ord
               , Generic
               , IsString
               , FromJSON
               , ToJSON
               , FromJSONKey
               , ToJSONKey
               )

instance Validity PropertyName

newtype PropertyValue = PropertyValue
    { propertyValueText :: Text
    } deriving ( Show
               , Eq
               , Ord
               , Generic
               , IsString
               , FromJSON
               , ToJSON
               , FromJSONKey
               , ToJSONKey
               )

instance Validity PropertyValue

newtype TimestampName = TimestampName
    { timestampNameText :: Text
    } deriving ( Show
               , Eq
               , Ord
               , Generic
               , IsString
               , FromJSON
               , ToJSON
               , FromJSONKey
               , ToJSONKey
               )

instance Validity TimestampName

data Timestamp
    = TimestampDay Day
    | TimestampTime UTCTime
    deriving (Show, Eq, Ord, Generic)

instance Validity Timestamp

instance FromJSON Timestamp where
    parseJSON =
        withObject "Timestamp" $ \o -> do
            p <- o .: "precision"
            case (p :: Text) of
                "day" -> do
                    s <- o .: "value"
                    TimestampDay <$>
                        parseTimeM False defaultTimeLocale timestampDayFormat s
                "time" -> do
                    s <- o .: "value"
                    (TimestampTime <$>
                     parseTimeM
                         False
                         defaultTimeLocale
                         timestampTimeExactFormat
                         s) <|>
                        (TimestampTime <$>
                         parseTimeM
                             False
                             defaultTimeLocale
                             timestampTimeFormat
                             s)
                _ -> fail "unknown precision"

instance ToJSON Timestamp where
    toJSON ts =
        let p :: Text
            v :: Value
            (p, v) =
                case ts of
                    TimestampDay d ->
                        ( "day"
                        , toJSON $
                          formatTime defaultTimeLocale timestampDayFormat d)
                    TimestampTime lt ->
                        ( "time"
                        , toJSON $
                          formatTime
                              defaultTimeLocale
                              timestampTimeExactFormat
                              lt)
         in object ["precision" .= p, "value" .= v]

timestampDayFormat :: String
timestampDayFormat = "%F"

timestampTimeFormat :: String
timestampTimeFormat = "%F %R"

timestampTimeExactFormat :: String
timestampTimeExactFormat = "%F %R %q"

newtype TodoState = TodoState
    { todoStateText :: Text
    } deriving (Show, Eq, Ord, Generic, IsString, FromJSON, ToJSON)

instance Validity TodoState

newtype StateHistory = StateHistory
    { unStateHistory :: [StateHistoryEntry]
    } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance Validity StateHistory

data StateHistoryEntry = StateHistoryEntry
    { stateHistoryEntryNewState :: Maybe TodoState
    , stateHistoryEntryTimestamp :: UTCTime
    } deriving (Show, Eq, Ord, Generic)

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

newtype Tag = Tag
    { tagText :: Text
    } deriving (Show, Eq, Ord, Generic, IsString, FromJSON, ToJSON)

instance Validity Tag

data Logbook
    = LogOpen UTCTime
              [LogbookEntry]
    | LogClosed [LogbookEntry]
    deriving (Show, Eq, Ord, Generic)

instance Validity Logbook

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
                pure $
                    case mend of
                        Nothing -> LogOpen start rest
                        Just end -> LogClosed $ LogbookEntry start end : rest

instance ToJSON Logbook where
    toJSON = toJSON . go
      where
        go (LogOpen start rest) = object ["start" .= start] : map toJSON rest
        go (LogClosed rest) = map toJSON rest

emptyLogbook :: Logbook
emptyLogbook = LogClosed []

data LogbookEntry = LogbookEntry
    { logbookEntryStart :: UTCTime
    , logbookEntryEnd :: UTCTime
    } deriving (Show, Eq, Ord, Generic)

instance Validity LogbookEntry

instance FromJSON LogbookEntry where
    parseJSON =
        withObject "LogbookEntry" $ \o ->
            LogbookEntry <$> o .: "start" <*> o .: "end"

instance ToJSON LogbookEntry where
    toJSON LogbookEntry {..} =
        object ["start" .= logbookEntryStart, "end" .= logbookEntryEnd]
