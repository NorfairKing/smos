{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Data.Types
  ( oldestParsableDataVersion,
    currentDataVersion,
    newestParsableDataVersion,
    Versioned (..),
    SmosFile (..),
    Forest,
    Tree (..),
    Entry (..),
    newEntry,
    emptyEntry,
    Header (..),
    emptyHeader,
    header,
    parseHeader,
    validHeaderChar,
    validateHeaderChar,
    Contents (..),
    emptyContents,
    nullContents,
    contents,
    parseContents,
    validContentsChar,
    validateContentsChar,
    PropertyName (..),
    emptyPropertyName,
    propertyName,
    parsePropertyName,
    validPropertyNameChar,
    PropertyValue (..),
    emptyPropertyValue,
    propertyValue,
    parsePropertyValue,
    validPropertyValueChar,
    TodoState (..),
    todoState,
    parseTodoState,
    validTodoStateChar,
    validateTodoStateChar,
    StateHistory (..),
    StateHistoryEntry (..),
    emptyStateHistory,
    nullStateHistory,
    Tag (..),
    emptyTag,
    tag,
    parseTag,
    validTagChar,
    validateTagChar,
    Logbook (..),
    emptyLogbook,
    nullLogbook,
    logbookOpen,
    logbookClosed,
    LogbookEntry (..),
    logbookEntryDiffTime,
    TimestampName (..),
    timestampName,
    parseTimestampName,
    emptyTimestampName,
    validTimestampNameChar,
    validateTimestampNameChar,
    Timestamp (..),
    timestampString,
    timestampText,
    timestampPrettyString,
    timestampPrettyText,
    dayCodec,
    timestampDayFormat,
    localTimeCodec,
    timestampLocalTimeFormat,
    timestampLocalTimePrettyFormat,
    parseTimestampString,
    parseTimestampText,
    timestampDay,
    timestampLocalTime,
    utctimeFormat,
    utctimeCodec,
    getLocalTime,
  )
where

import Autodocodec
import Autodocodec.Yaml
import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..))
import qualified Data.Aeson as JSON
import Data.Char as Char
import Data.Function
import Data.Functor.Classes (Ord1 (..))
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.SemVer as Version
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Tree
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Yaml.Builder (ToYaml (..))
import GHC.Generics (Generic)

oldestParsableDataVersion :: Version
oldestParsableDataVersion = version 0 0 0 [] []

currentDataVersion :: Version
currentDataVersion = version 1 0 0 [] []

newestParsableDataVersion :: Version
newestParsableDataVersion = version 1 0 0 [] []

instance Validity Version where
  validate = trivialValidation

instance ToJSON Version where
  toJSON = toJSONViaCodec
  toEncoding = toEncodingViaCodec

instance FromJSON Version where
  parseJSON = parseJSONViaCodec

instance ToYaml Version where
  toYaml = toYamlViaCodec

instance HasCodec Version where
  codec = bimapCodec Version.fromText Version.toText codec

-- | A versioned value
data Versioned a = Versioned
  { versionedVersion :: !Version,
    versionedValue :: !a
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON, ToYaml) via (Autodocodec (Versioned a))

instance Validity a => Validity (Versioned a)

instance HasCodec a => HasCodec (Versioned a) where
  codec =
    object "Versioned" $
      Versioned
        <$> requiredField "version" "version" .= versionedVersion
        <*> requiredField "value" "versioned value" .= versionedValue

newtype SmosFile = SmosFile
  { smosFileForest :: Forest Entry
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON, ToYaml) via (Autodocodec SmosFile)

instance Ord SmosFile where
  compare = liftCompare (liftCompare compare) `on` smosFileForest

instance Validity SmosFile

instance NFData SmosFile

instance HasCodec SmosFile where
  codec = dimapCodec SmosFile smosFileForest entryForestCodec

entryTreeCodec :: JSONCodec (Tree Entry)
entryTreeCodec =
  named "Tree Entry" $
    dimapCodec f g $
      eitherCodec (codec <?> "Leaf entry") $
        object "Tree Entry" $
          Node
            <$> requiredField "entry" "root entry" .= rootLabel
            <*> optionalFieldWithOmittedDefaultWith "forest" entryForestCodec [] "sub forest" .= subForest
  where
    f = \case
      Left e -> Node e []
      Right tree -> tree
    g tree@(Node e subforest) = case subforest of
      [] -> Left e
      _ -> Right tree

entryForestCodec :: JSONCodec (Forest Entry)
entryForestCodec = named "Forest Entry" $ listCodec entryTreeCodec

utctimeFormat :: String
utctimeFormat = "%F %H:%M:%S.%q"

utctimeCodec :: JSONCodec UTCTime
utctimeCodec =
  bimapCodec
    (parseTimeEither defaultTimeLocale utctimeFormat)
    (formatTime defaultTimeLocale utctimeFormat)
    codec
    <?> T.pack utctimeFormat

data Entry = Entry
  { entryHeader :: Header,
    entryContents :: Maybe Contents,
    entryTimestamps :: Map TimestampName Timestamp, -- SCHEDULED, DEADLINE, etc.
    entryProperties :: Map PropertyName PropertyValue,
    entryStateHistory :: StateHistory, -- TODO, DONE, etc.
    entryTags :: Set Tag, -- '@home', 'toast', etc.
    entryLogbook :: Logbook
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON, ToYaml) via (Autodocodec Entry)

instance Validity Entry

instance NFData Entry

instance HasCodec Entry where
  codec =
    named "Entry" $
      dimapCodec f g $
        eitherCodec (codec <?> "only a header") $
          object "Entry" $
            Entry
              <$> requiredField "header" "header" .= entryHeader
              <*> optionalField "contents" "contents" .= entryContents
              <*> optionalFieldWithOmittedDefault "timestamps" M.empty "timestamps" .= entryTimestamps
              <*> optionalFieldWithOmittedDefault "properties" M.empty "properties" .= entryProperties
              <*> optionalFieldWithOmittedDefault "state-history" emptyStateHistory "state history" .= entryStateHistory
              <*> optionalFieldWithOmittedDefault "tags" S.empty "tags" .= entryTags
              <*> optionalFieldWithOmittedDefault "logbook" emptyLogbook "logbook" .= entryLogbook
    where
      f = \case
        Left h -> newEntry h
        Right e -> e
      g e@Entry {..} =
        if and
          [ isNothing entryContents,
            M.null entryTimestamps,
            M.null entryProperties,
            nullStateHistory entryStateHistory,
            null entryTags,
            entryLogbook == emptyLogbook
          ]
          then Left entryHeader
          else Right e

newEntry :: Header -> Entry
newEntry h =
  Entry
    { entryHeader = h,
      entryContents = Nothing,
      entryTimestamps = M.empty,
      entryProperties = M.empty,
      entryStateHistory = emptyStateHistory,
      entryTags = S.empty,
      entryLogbook = emptyLogbook
    }

emptyEntry :: Entry
emptyEntry = newEntry emptyHeader

newtype Header = Header
  { headerText :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString)
  deriving (FromJSON, ToJSON, ToYaml) via (Autodocodec Header)

instance Validity Header where
  validate (Header t) = mconcat [delve "headerText" t, decorateList (T.unpack t) validateHeaderChar]

instance NFData Header

instance HasCodec Header where
  codec = bimapCodec parseHeader headerText codec

emptyHeader :: Header
emptyHeader = Header ""

header :: Text -> Maybe Header
header = constructValid . Header

parseHeader :: Text -> Either String Header
parseHeader = prettyValidate . Header

validHeaderChar :: Char -> Bool
validHeaderChar = validationIsValid . validateHeaderChar

validateHeaderChar :: Char -> Validation
validateHeaderChar c =
  mconcat
    [ declare "The character is printable" $ isPrint c,
      declare "The character is not a newline" $ c /= '\n'
    ]

newtype Contents = Contents
  { contentsText :: Text
  }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (IsString)
  deriving (FromJSON, ToJSON, ToYaml) via (Autodocodec Contents)

instance Validity Contents where
  validate (Contents t) =
    mconcat [delve "contentsText" t, decorateList (T.unpack t) validateContentsChar]

instance NFData Contents

instance HasCodec Contents where
  codec = bimapCodec parseContents contentsText codec

emptyContents :: Contents
emptyContents = Contents ""

nullContents :: Contents -> Bool
nullContents = (== emptyContents)

contents :: Text -> Maybe Contents
contents = constructValid . Contents

parseContents :: Text -> Either String Contents
parseContents = prettyValidate . Contents

validContentsChar :: Char -> Bool
validContentsChar = validationIsValid . validateContentsChar

validateContentsChar :: Char -> Validation
validateContentsChar c =
  declare "The character is a printable or space" $ Char.isPrint c || Char.isSpace c

newtype PropertyName = PropertyName
  { propertyNameText :: Text
  }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (IsString, ToJSONKey)
  deriving (FromJSON, ToJSON, ToYaml) via (Autodocodec PropertyName)

instance Validity PropertyName where
  validate (PropertyName t) =
    mconcat [delve "propertyNameText" t, decorateList (T.unpack t) validatePropertyNameChar]

instance NFData PropertyName

instance FromJSONKey PropertyName where
  fromJSONKey = JSON.FromJSONKeyTextParser parseJSONPropertyName

instance HasCodec PropertyName where
  codec = bimapCodec parsePropertyName propertyNameText codec

parseJSONPropertyName :: MonadFail m => Text -> m PropertyName
parseJSONPropertyName t =
  case propertyName t of
    Nothing -> fail $ "Invalid property name: " <> T.unpack t
    Just h -> pure h

emptyPropertyName :: PropertyName
emptyPropertyName = PropertyName ""

propertyName :: Text -> Maybe PropertyName
propertyName = constructValid . PropertyName

parsePropertyName :: Text -> Either String PropertyName
parsePropertyName = prettyValidate . PropertyName

validPropertyNameChar :: Char -> Bool
validPropertyNameChar = validationIsValid . validatePropertyNameChar

validatePropertyNameChar :: Char -> Validation
validatePropertyNameChar = validateTagChar

newtype PropertyValue = PropertyValue
  { propertyValueText :: Text
  }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (IsString, ToJSONKey)
  deriving (FromJSON, ToJSON, ToYaml) via (Autodocodec PropertyValue)

instance Validity PropertyValue where
  validate (PropertyValue t) =
    mconcat
      [ delve "propertyValueText" t,
        decorateList (T.unpack t) $ \c ->
          declare "The character is a valid property value character" $ validPropertyValueChar c
      ]

instance NFData PropertyValue

instance HasCodec PropertyValue where
  codec = bimapCodec parsePropertyValue propertyValueText codec

emptyPropertyValue :: PropertyValue
emptyPropertyValue = PropertyValue ""

propertyValue :: Text -> Maybe PropertyValue
propertyValue = constructValid . PropertyValue

parsePropertyValue :: Text -> Either String PropertyValue
parsePropertyValue = prettyValidate . PropertyValue

validPropertyValueChar :: Char -> Bool
validPropertyValueChar = validHeaderChar

newtype TimestampName = TimestampName
  { timestampNameText :: Text
  }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (IsString, ToJSONKey)
  deriving (FromJSON, ToJSON, ToYaml) via (Autodocodec TimestampName)

instance Validity TimestampName where
  validate (TimestampName t) =
    mconcat [delve "timestampNameText" t, decorateList (T.unpack t) validateTimestampNameChar]

instance NFData TimestampName

instance FromJSONKey TimestampName where
  fromJSONKey = JSON.FromJSONKeyTextParser parseJSONTimestampName

instance HasCodec TimestampName where
  codec = bimapCodec parseTimestampName timestampNameText codec

parseJSONTimestampName :: MonadFail m => Text -> m TimestampName
parseJSONTimestampName t =
  case timestampName t of
    Nothing -> fail $ "Invalid timestamp name: " <> T.unpack t
    Just h -> pure h

emptyTimestampName :: TimestampName
emptyTimestampName = TimestampName ""

timestampName :: Text -> Maybe TimestampName
timestampName = constructValid . TimestampName

parseTimestampName :: Text -> Either String TimestampName
parseTimestampName = prettyValidate . TimestampName

validTimestampNameChar :: Char -> Bool
validTimestampNameChar = validationIsValid . validateTimestampNameChar

validateTimestampNameChar :: Char -> Validation
validateTimestampNameChar = validateHeaderChar

data Timestamp
  = TimestampDay Day
  | TimestampLocalTime LocalTime
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON, ToYaml) via (Autodocodec Timestamp)

instance Validity Timestamp

instance NFData Timestamp

instance HasCodec Timestamp where
  codec = dimapCodec f g $ eitherCodec dayCodec localTimeCodec
    where
      f = \case
        Left d -> TimestampDay d
        Right lt -> TimestampLocalTime lt
      g = \case
        TimestampDay d -> Left d
        TimestampLocalTime lt -> Right lt

dayCodec :: JSONCodec Day
dayCodec =
  bimapCodec
    (parseTimeEither defaultTimeLocale timestampDayFormat)
    (formatTime defaultTimeLocale timestampDayFormat)
    codec
    <?> T.pack timestampDayFormat

timestampDayFormat :: String
timestampDayFormat = "%F"

localTimeCodec :: JSONCodec LocalTime
localTimeCodec =
  bimapCodec
    (parseTimeEither defaultTimeLocale timestampLocalTimeFormat)
    (formatTime defaultTimeLocale timestampLocalTimeFormat)
    codec
    <?> T.pack timestampLocalTimeFormat

timestampLocalTimeFormat :: String
timestampLocalTimeFormat = "%F %T%Q"

timestampLocalTimePrettyFormat :: String
timestampLocalTimePrettyFormat = "%F %T"

timestampString :: Timestamp -> String
timestampString ts =
  case ts of
    TimestampDay d -> formatTime defaultTimeLocale timestampDayFormat d
    TimestampLocalTime lt -> formatTime defaultTimeLocale timestampLocalTimeFormat lt

timestampText :: Timestamp -> Text
timestampText = T.pack . timestampString

timestampPrettyString :: Timestamp -> String
timestampPrettyString ts =
  case ts of
    TimestampDay d -> formatTime defaultTimeLocale timestampDayFormat d
    TimestampLocalTime lt -> formatTime defaultTimeLocale timestampLocalTimePrettyFormat lt

timestampPrettyText :: Timestamp -> Text
timestampPrettyText = T.pack . timestampPrettyString

parseTimestampString :: String -> Maybe Timestamp
parseTimestampString s =
  (TimestampDay <$> parseTimeM False defaultTimeLocale timestampDayFormat s)
    <|> (TimestampLocalTime <$> parseTimeM False defaultTimeLocale timestampLocalTimeFormat s)

parseTimestampText :: Text -> Maybe Timestamp
parseTimestampText = parseTimestampString . T.unpack

timestampDay :: Timestamp -> Day
timestampDay ts =
  case ts of
    TimestampDay d -> d
    TimestampLocalTime (LocalTime d _) -> d

timestampLocalTime :: Timestamp -> LocalTime
timestampLocalTime ts =
  case ts of
    TimestampDay d -> LocalTime d midnight
    TimestampLocalTime lt -> lt

newtype TodoState = TodoState
  { todoStateText :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString)
  deriving (FromJSON, ToJSON, ToYaml) via (Autodocodec TodoState)

instance Validity TodoState where
  validate (TodoState t) =
    mconcat [delve "todoStateText" t, decorateList (T.unpack t) validateTodoStateChar]

instance NFData TodoState

instance HasCodec TodoState where
  codec = bimapCodec parseTodoState todoStateText codec

todoState :: Text -> Maybe TodoState
todoState = constructValid . TodoState

parseTodoState :: Text -> Either String TodoState
parseTodoState = prettyValidate . TodoState

validTodoStateChar :: Char -> Bool
validTodoStateChar = validationIsValid . validateTodoStateChar

validateTodoStateChar :: Char -> Validation
validateTodoStateChar = validateHeaderChar

newtype StateHistory = StateHistory
  { unStateHistory :: [StateHistoryEntry]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON, ToYaml) via (Autodocodec StateHistory)

instance Validity StateHistory where
  validate st@(StateHistory hs) =
    genericValidate st
      <> declare "The entries are stored in reverse chronological order" (hs <= sort hs)

instance NFData StateHistory

instance HasCodec StateHistory where
  codec = dimapCodec StateHistory unStateHistory codec <?> "In reverse chronological order"

emptyStateHistory :: StateHistory
emptyStateHistory = StateHistory []

nullStateHistory :: StateHistory -> Bool
nullStateHistory = (== emptyStateHistory)

data StateHistoryEntry = StateHistoryEntry
  { stateHistoryEntryNewState :: Maybe TodoState,
    stateHistoryEntryTimestamp :: UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON, ToYaml) via (Autodocodec StateHistoryEntry)

instance Validity StateHistoryEntry

instance NFData StateHistoryEntry

instance Ord StateHistoryEntry where
  compare =
    mconcat [comparing $ Down . stateHistoryEntryTimestamp, comparing stateHistoryEntryNewState]

instance HasCodec StateHistoryEntry where
  codec =
    object "StateHistoryEntry" $
      StateHistoryEntry
        <$> requiredField "state" "new state" .= stateHistoryEntryNewState
        <*> requiredFieldWith "time" utctimeCodec "time at which the state change happened" .= stateHistoryEntryTimestamp

-- TODO backward compatibility?
--     objectParser "StateHistoryEntry" $
--       StateHistoryEntry
--         <$> ( requiredField "state" "The new state"
--                 <|> requiredField "new-state" "legacy key"
--             )
--         <*> ( unForYaml
--                 <$> ( requiredField "time" "The time at which the change happened"
--                         <|> requiredField "timestamp" "legacy key"
--                     )
--             )

newtype Tag = Tag
  { tagText :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString)
  deriving (FromJSON, ToJSON, ToYaml) via (Autodocodec Tag)

instance Validity Tag where
  validate (Tag t) = mconcat [delve "tagText" t, decorateList (T.unpack t) validateTagChar]

instance NFData Tag

instance HasCodec Tag where
  codec = bimapCodec parseTag tagText codec

emptyTag :: Tag
emptyTag = Tag ""

tag :: Text -> Maybe Tag
tag = constructValid . Tag

parseTag :: Text -> Either String Tag
parseTag = prettyValidate . Tag

validTagChar :: Char -> Bool
validTagChar = validationIsValid . validateTagChar

validateTagChar :: Char -> Validation
validateTagChar c =
  mconcat
    [ validateHeaderChar c,
      declare "The character is not whitespace" $ not $ Char.isSpace c,
      declare "The character is not a parenthesis" $ c `notElem` ['(', ')']
    ]

data Logbook
  = LogOpen UTCTime [LogbookEntry]
  | LogClosed [LogbookEntry]
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON, ToYaml) via (Autodocodec Logbook)

instance Validity Logbook where
  validate lo@(LogOpen utct lbes) =
    mconcat
      [ genericValidate lo,
        declare "The open time occurred after the last entry ended" $
          case lbes of
            [] -> True
            (lbe : _) -> utct >= logbookEntryEnd lbe,
        decorate "The consecutive logbook entries happen after each other" $
          decorateList (conseqs lbes) $
            \(lbe1, lbe2) ->
              declare "The former happens after the latter" $
                logbookEntryStart lbe1 >= logbookEntryEnd lbe2
      ]
  validate lc@(LogClosed lbes) =
    mconcat
      [ genericValidate lc,
        decorate "The consecutive logbook entries happen after each other" $
          decorateList (conseqs lbes) $
            \(lbe1, lbe2) ->
              declare "The former happens after the latter" $
                logbookEntryStart lbe1 >= logbookEntryEnd lbe2
      ]

conseqs :: [a] -> [(a, a)]
conseqs [] = []
conseqs [_] = []
conseqs (a : b : as) = (a, b) : conseqs (b : as)

instance NFData Logbook

instance HasCodec Logbook where
  codec =
    bimapCodec prettyValidate id $
      bimapCodec f g (listCodec tupCodec)
        <??> [ "Logbook entries, in reverse chronological order.",
               "Only the first element of this list has an optional 'end'."
             ]
    where
      tupCodec :: JSONCodec (UTCTime, Maybe UTCTime)
      tupCodec =
        object "LogbookEntry" $
          (,)
            <$> requiredFieldWith "start" utctimeCodec "start of the logbook entry" .= fst
            <*> optionalFieldWith "end" utctimeCodec "end of the logbook entry" .= snd
      f es = case NE.nonEmpty es of
        Nothing -> Right $ LogClosed []
        Just ((start, mEnd) :| rest) -> do
          let firstFunc = case mEnd of
                Nothing -> LogOpen start
                Just end -> LogClosed . ((LogbookEntry {logbookEntryStart = start, logbookEntryEnd = end}) :)
          es_ <- forM rest $ \(start_, mEnd_) -> case mEnd_ of
            Nothing -> Left "Missing 'end' in logbook."
            Just end -> Right $ LogbookEntry {logbookEntryStart = start_, logbookEntryEnd = end}
          pure $ firstFunc es_
      g = \case
        LogOpen start es -> (start, Nothing) : map (\LogbookEntry {..} -> (logbookEntryStart, Just logbookEntryEnd)) es
        LogClosed es -> map (\LogbookEntry {..} -> (logbookEntryStart, Just logbookEntryEnd)) es

emptyLogbook :: Logbook
emptyLogbook = LogClosed []

nullLogbook :: Logbook -> Bool
nullLogbook = (== emptyLogbook)

logbookOpen :: Logbook -> Bool
logbookOpen =
  \case
    LogOpen _ _ -> True
    _ -> False

logbookClosed :: Logbook -> Bool
logbookClosed =
  \case
    LogClosed _ -> True
    _ -> False

data LogbookEntry = LogbookEntry
  { logbookEntryStart :: UTCTime,
    logbookEntryEnd :: UTCTime
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON, ToYaml) via (Autodocodec LogbookEntry)

instance Validity LogbookEntry where
  validate lbe@LogbookEntry {..} =
    mconcat
      [ genericValidate lbe,
        declare "The start time occurred before the end time" $ logbookEntryStart <= logbookEntryEnd
      ]

instance NFData LogbookEntry

instance HasCodec LogbookEntry where
  codec =
    bimapCodec prettyValidate id $
      object "LogbookEntry" $
        LogbookEntry
          <$> requiredFieldWith "start" utctimeCodec "start of the logbook entry" .= logbookEntryStart
          <*> requiredFieldWith "end" utctimeCodec "end of the logbook entry" .= logbookEntryEnd

logbookEntryDiffTime :: LogbookEntry -> NominalDiffTime
logbookEntryDiffTime LogbookEntry {..} = diffUTCTime logbookEntryEnd logbookEntryStart

getLocalTime :: IO LocalTime
getLocalTime = (\zt -> utcToLocalTime (zonedTimeZone zt) (zonedTimeToUTC zt)) <$> getZonedTime

parseTimeEither :: ParseTime a => TimeLocale -> String -> String -> Either String a
parseTimeEither locale format string = case parseTimeM True locale format string of
  Nothing -> Left $ "Failed to parse time value: " <> string <> " via " <> format
  Just r -> Right r
