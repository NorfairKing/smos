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
    entryForestCodec,
    entryTreeCodec,
    Entry (..),
    newEntry,
    entryWithState,
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
    parseTimestampString,
    parseTimestampText,
    timestampDay,
    timestampLocalTime,
    dayCodec,
    parseDayString,
    renderDayString,
    localTimeCodec,
    parseLocalTimeString,
    renderLocalTimeString,
    utctimeCodec,
    parseUTCTimeString,
    renderUTCTimeString,
    getLocalTime,
    parseTimeEither,
    mkImpreciseUTCTime,
    validateImpreciseUTCTime,
    mkImpreciseLocalTime,
    validateImpreciseLocalTime,
    mkImpreciseTimeOfDay,
    validateImpreciseTimeOfDay,
    parseTZLabel,
    renderTZLabel,
  )
where

import Autodocodec
import Autodocodec.Yaml
import Control.Applicative
import Control.Arrow (left)
import Control.DeepSeq
import Control.Monad
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..))
import qualified Data.Aeson as JSON
import Data.Char as Char
import Data.Function
import Data.Functor.Classes (Ord1 (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import Data.SemVer as Version
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Time.Zones
import Data.Time.Zones.All
import Data.Tree
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Yaml.Builder (ToYaml (..))
import GHC.Generics (Generic)
import Path

oldestParsableDataVersion :: Version
oldestParsableDataVersion = version 0 0 0 [] []

currentDataVersion :: Version
currentDataVersion = version 2 0 0 [] []

newestParsableDataVersion :: Version
newestParsableDataVersion = version 2 0 0 [] []

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
  codec = named "Version" $ bimapCodec Version.fromText Version.toText codec

-- | A versioned value
data Versioned a = Versioned
  { versionedVersion :: !Version,
    versionedValue :: !a
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON, ToYaml) via (Autodocodec (Versioned a))

instance (Validity a) => Validity (Versioned a)

instance (HasCodec a) => HasCodec (Versioned a) where
  codec =
    object "Versioned" $
      Versioned
        <$> requiredField "version" "version"
          .= versionedVersion
        <*> requiredField "value" "versioned value"
          .= versionedValue

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
  codec = named "SmosFile" $ dimapCodec SmosFile smosFileForest $ entryForestCodec "Entry" codec

entryTreeCodec :: (Eq a) => Text -> JSONCodec a -> JSONCodec (Tree a)
entryTreeCodec n c =
  named ("Tree " <> n) $
    dimapCodec f g $
      eitherCodec
        -- We must use the object codec here as the left side, because whether
        -- the 'entry' key is in the object will determine whether we're
        -- parsing an entry or a forest.
        -- If we did it the other way around, then an entry tree object would
        -- look like an entry with all default values.
        ( object "Tree Entry" $
            Node
              <$> requiredFieldWith "entry" c "root"
                .= rootLabel
              <*> optionalFieldWithOmittedDefaultWith "forest" (entryForestCodec n c) [] "subforest"
                .= subForest
        )
        (c <?> "Leaf entry")
  where
    f = \case
      Right e -> Node e []
      Left tree -> tree
    g tree@(Node e subforest) = case subforest of
      [] -> Right e
      _ -> Left tree

entryForestCodec :: (Eq a) => Text -> JSONCodec a -> JSONCodec (Forest a)
entryForestCodec n c = named ("Forest " <> n) $ listCodec $ entryTreeCodec n c

data Entry = Entry
  { entryHeader :: !Header,
    entryContents :: !(Maybe Contents),
    entryTimestamps :: !(Map TimestampName Timestamp), -- SCHEDULED, DEADLINE, etc.
    entryProperties :: !(Map PropertyName PropertyValue),
    entryStateHistory :: !StateHistory, -- TODO, DONE, etc.
    entryTags :: !(Set Tag), -- '@home', 'toast', etc.
    entryLogbook :: !Logbook
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
              <$> headerField
              <*> contentsField
              <*> timestampsField
              <*> propertiesField
              <*> stateHistoryField
              <*> tagsField
              <*> logbookField
    where
      headerField = requiredField "header" "header" .= entryHeader
      contentsField = optionalField "contents" "contents" .= entryContents
      timestampsField = optionalFieldWithOmittedDefault "timestamps" M.empty "timestamps" .= entryTimestamps
      propertiesField = optionalFieldWithOmittedDefault "properties" M.empty "properties" .= entryProperties
      stateHistoryField =
        dimapCodec
          (\(msh1, msh2) -> fromMaybe emptyStateHistory $ msh1 <|> msh2)
          (\sh -> if nullStateHistory sh then (Nothing, Nothing) else (Just sh, Nothing))
          ( (,)
              <$> oldStateHistoryField
                .= fst
              <*> newStateHistoryField
                .= snd
          )
          .= entryStateHistory
      newStateHistoryField = optionalField "history" "state history"
      oldStateHistoryField = optionalField "state-history" "state history (legacy key)"
      tagsField = optionalFieldWithOmittedDefault "tags" S.empty "tags" .= entryTags
      logbookField = optionalFieldWithOmittedDefault "logbook" emptyLogbook "logbook" .= entryLogbook
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

entryWithState :: Header -> UTCTime -> TodoState -> Entry
entryWithState h time state =
  (newEntry h)
    { entryStateHistory =
        StateHistory
          [ StateHistoryEntry
              { stateHistoryEntryNewState = Just state,
                stateHistoryEntryTimestamp = time
              }
          ]
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
  validate (Header t) =
    mconcat
      [ delve "headerText" t,
        decorateList (T.unpack t) validateHeaderChar
      ]

instance NFData Header

instance HasCodec Header where
  codec =
    named "Header" $
      bimapCodec parseHeader headerText codec

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
    mconcat
      [ delve "contentsText" t,
        decorateList (T.unpack t) validateContentsChar
      ]

instance NFData Contents

instance HasCodec Contents where
  codec =
    named "Contents" $
      bimapCodec parseContents contentsText codec

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
    mconcat
      [ delve "propertyNameText" t,
        decorateList (T.unpack t) validatePropertyNameChar
      ]

instance NFData PropertyName

instance FromJSONKey PropertyName where
  fromJSONKey = JSON.FromJSONKeyTextParser parseJSONPropertyName

instance HasCodec PropertyName where
  codec =
    named "PropertyName" $
      bimapCodec parsePropertyName propertyNameText codec

parseJSONPropertyName :: (MonadFail m) => Text -> m PropertyName
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
  codec =
    named "PropertyValue" $
      bimapCodec parsePropertyValue propertyValueText codec

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
    mconcat
      [ delve "timestampNameText" t,
        decorateList (T.unpack t) validateTimestampNameChar
      ]

instance NFData TimestampName

instance FromJSONKey TimestampName where
  fromJSONKey = JSON.FromJSONKeyTextParser parseJSONTimestampName

instance HasCodec TimestampName where
  codec =
    named "TimestampName" $
      bimapCodec parseTimestampName timestampNameText codec
        <??> [ "Name of a Timestamp",
               "You can any timestamp name you like, but by convention they are all upper-case.",
               "Some standard timestamp names provide semantics for timestamps:",
               "AFTER: This entry will only become relevant after the given timestamp.",
               "BEGIN: This entry describes an event that begins on the given timestamp (inclusive).",
               "END: This entry describes an event that ends on the given timestamp (exclusive).",
               "DEADLINE: This entry describes a task that needs to be done before the given timestamp (exclusive).",
               "SCHEDULED: This entry describes an occurrence that happens at a given timestamp."
             ]

parseJSONTimestampName :: (MonadFail m) => Text -> m TimestampName
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
  = TimestampDay !Day
  | TimestampLocalTime !LocalTime
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON, ToYaml) via (Autodocodec Timestamp)

instance Validity Timestamp where
  validate ts =
    mconcat
      [ genericValidate ts,
        case ts of
          TimestampDay _ -> valid
          TimestampLocalTime lt -> validateImpreciseLocalTime lt
      ]

instance NFData Timestamp

instance HasCodec Timestamp where
  codec =
    named "Timestamp" $
      dimapCodec f g $
        eitherCodec dayCodec localTimeCodec
    where
      f = \case
        Left d -> TimestampDay d
        Right lt -> TimestampLocalTime lt
      g = \case
        TimestampDay d -> Left d
        TimestampLocalTime lt -> Right lt

timestampString :: Timestamp -> String
timestampString ts =
  case ts of
    TimestampDay d -> renderDayString d
    TimestampLocalTime lt -> renderLocalTimeString lt

timestampText :: Timestamp -> Text
timestampText = T.pack . timestampString

timestampPrettyString :: Timestamp -> String
timestampPrettyString ts =
  case ts of
    TimestampDay d -> renderDayString d
    TimestampLocalTime lt -> formatTime defaultTimeLocale "%F %T" lt

timestampPrettyText :: Timestamp -> Text
timestampPrettyText = T.pack . timestampPrettyString

parseTimestampString :: String -> Either String Timestamp
parseTimestampString s =
  (TimestampDay <$> parseDayString s)
    <|> (TimestampLocalTime <$> parseLocalTimeString s)

parseTimestampText :: Text -> Either String Timestamp
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
    mconcat
      [ delve "todoStateText" t,
        decorateList (T.unpack t) validateTodoStateChar
      ]

instance NFData TodoState

instance HasCodec TodoState where
  codec =
    named "TodoState" $
      bimapCodec parseTodoState todoStateText codec

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
    mconcat
      [ genericValidate st,
        declare "The entries are stored in reverse chronological order" $
          all
            (\(a, b) -> stateHistoryEntryTimestamp a >= stateHistoryEntryTimestamp b)
            (conseqs hs)
      ]

instance NFData StateHistory

instance HasCodec StateHistory where
  codec =
    named "StateHistory" $
      bimapCodec
        prettyValidate
        id
        (dimapCodec StateHistory unStateHistory codec)
        <?> "In reverse chronological order"

emptyStateHistory :: StateHistory
emptyStateHistory = StateHistory []

nullStateHistory :: StateHistory -> Bool
nullStateHistory = (== emptyStateHistory)

data StateHistoryEntry = StateHistoryEntry
  { stateHistoryEntryNewState :: !(Maybe TodoState),
    stateHistoryEntryTimestamp :: !UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON, ToYaml) via (Autodocodec StateHistoryEntry)

instance Validity StateHistoryEntry where
  validate she@StateHistoryEntry {..} =
    mconcat
      [ genericValidate she,
        decorate "stateHistoryEntryTimestamp" $ validateImpreciseUTCTime stateHistoryEntryTimestamp
      ]

instance NFData StateHistoryEntry

instance Ord StateHistoryEntry where
  compare =
    mconcat
      [ comparing $ Down . stateHistoryEntryTimestamp,
        comparing stateHistoryEntryNewState
      ]

instance HasCodec StateHistoryEntry where
  codec =
    named "StateHistoryEntry" $
      parseAlternative
        ( object "StateHistoryEntry" $
            StateHistoryEntry
              <$> requiredField "state" "new state"
                .= stateHistoryEntryNewState
              <*> requiredFieldWith "time" utctimeCodec "time at which the state change happened"
                .= stateHistoryEntryTimestamp
        )
        ( object "StateHistoryEntry (legacy)" $
            StateHistoryEntry
              <$> requiredField "new-state" "new state"
                .= stateHistoryEntryNewState
              <*> requiredFieldWith "timestamp" utctimeCodec "time at which the state change happened"
                .= stateHistoryEntryTimestamp
        )

newtype Tag = Tag
  { tagText :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString)
  deriving (FromJSON, ToJSON, ToYaml) via (Autodocodec Tag)

instance Validity Tag where
  validate (Tag t) =
    mconcat
      [ delve "tagText" t,
        decorateList (T.unpack t) validateTagChar
      ]

instance NFData Tag

instance HasCodec Tag where
  codec =
    named "Tag" $
      bimapCodec parseTag tagText codec

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
  = LogOpen !UTCTime ![LogbookEntry]
  | LogClosed ![LogbookEntry]
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON, ToYaml) via (Autodocodec Logbook)

instance Validity Logbook where
  validate lb =
    mconcat
      [ genericValidate lb,
        let lbes = case lb of
              LogOpen _ es -> es
              LogClosed es -> es
         in decorate "The consecutive logbook entries happen after each other" $
              decorateList (conseqs lbes) $
                \(lbe1, lbe2) ->
                  declare "The former happens after the latter" $
                    logbookEntryStart lbe1 >= logbookEntryEnd lbe2,
        case lb of
          LogOpen u lbes ->
            mconcat
              [ decorate "The open time" $ validateImpreciseUTCTime u,
                declare "The open time occurred after the last entry ended" $
                  case lbes of
                    [] -> True
                    (lbe : _) -> u >= logbookEntryEnd lbe
              ]
          LogClosed _ -> valid
      ]

conseqs :: [a] -> [(a, a)]
conseqs [] = []
conseqs [_] = []
conseqs (a : b : as) = (a, b) : conseqs (b : as)

instance NFData Logbook

instance HasCodec Logbook where
  codec =
    named "Logbook" $
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
            <$> requiredFieldWith
              "start"
              utctimeCodec
              "start of the logbook entry"
              .= fst
            <*> optionalFieldWith
              "end"
              utctimeCodec
              "end of the logbook entry"
              .= snd
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
  { logbookEntryStart :: !UTCTime,
    logbookEntryEnd :: !UTCTime
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON, ToYaml) via (Autodocodec LogbookEntry)

instance Validity LogbookEntry where
  validate lbe@LogbookEntry {..} =
    mconcat
      [ genericValidate lbe,
        declare "The start time occurred before the end time" $ logbookEntryStart <= logbookEntryEnd,
        decorate "logbookEntryStart" $ validateImpreciseUTCTime logbookEntryStart,
        decorate "logbookEntryEnd" $ validateImpreciseUTCTime logbookEntryEnd
      ]

instance NFData LogbookEntry

instance HasCodec LogbookEntry where
  codec =
    named "LogbookEntry" $
      bimapCodec prettyValidate id $
        object "LogbookEntry" $
          LogbookEntry
            <$> requiredFieldWith "start" utctimeCodec "start of the logbook entry"
              .= logbookEntryStart
            <*> requiredFieldWith "end" utctimeCodec "end of the logbook entry"
              .= logbookEntryEnd

logbookEntryDiffTime :: LogbookEntry -> NominalDiffTime
logbookEntryDiffTime LogbookEntry {..} = diffUTCTime logbookEntryEnd logbookEntryStart

getLocalTime :: IO LocalTime
getLocalTime = (\zt -> utcToLocalTime (zonedTimeZone zt) (zonedTimeToUTC zt)) <$> getZonedTime

dayCodec :: JSONCodec Day
dayCodec =
  named "Day" $
    bimapCodec parseDayString renderDayString codec
      <?> T.pack dayFormat

dayFormat :: String
dayFormat = "%F"

parseDayString :: String -> Either String Day
parseDayString = parseTimeEither defaultTimeLocale dayFormat

renderDayString :: Day -> String
renderDayString = formatTime defaultTimeLocale dayFormat

utctimeCodec :: JSONCodec UTCTime
utctimeCodec =
  named "UTCTime" $
    bimapCodec parseUTCTimeString renderUTCTimeString codec
      <?> T.pack utctimeFormat

utctimeFormat :: String
utctimeFormat = "%F %T%Q"

parseUTCTimeString :: String -> Either String UTCTime
parseUTCTimeString s = fmap mkImpreciseUTCTime $
  case parseTimeEither defaultTimeLocale utctimeFormat s of
    Right u -> Right u
    Left err ->
      case parseTimeEither defaultTimeLocale "%FT%T%QZ" s <|> parseTimeEither defaultTimeLocale "%F %T%Q%z" s of
        Right u -> Right u
        Left _ -> Left err

renderUTCTimeString :: UTCTime -> String
renderUTCTimeString = formatTime defaultTimeLocale utctimeFormat

localTimeCodec :: JSONCodec LocalTime
localTimeCodec =
  named "LocalTime" $
    bimapCodec parseLocalTimeString renderLocalTimeString codec
      <?> T.pack localTimeFormat

localTimeFormat :: String
localTimeFormat = "%F %T%Q"

parseLocalTimeString :: String -> Either String LocalTime
parseLocalTimeString s = fmap mkImpreciseLocalTime $
  case parseTimeEither defaultTimeLocale localTimeFormat s of
    Right u -> Right u
    Left err -> case parseTimeEither defaultTimeLocale "%FT%T%QZ" s of
      Right u -> Right u
      Left _ -> Left err

renderLocalTimeString :: LocalTime -> String
renderLocalTimeString = formatTime defaultTimeLocale localTimeFormat

parseTimeEither :: (ParseTime a) => TimeLocale -> String -> String -> Either String a
parseTimeEither locale format string = case parseTimeM True locale format string of
  Nothing -> Left $ "Failed to parse time value: " <> string <> " via " <> format
  Just r -> Right r

instance HasCodec (Path Rel File) where
  codec = bimapCodec (left show . parseRelFile) fromRelFile codec <?> "relative filepath"

instance ToYaml (Path Rel File) where
  toYaml = toYamlViaCodec

mkImpreciseUTCTime :: UTCTime -> UTCTime
mkImpreciseUTCTime u = u {utctDayTime = fromIntegral (floor (utctDayTime u) :: Word)}

validateImpreciseUTCTime :: UTCTime -> Validation
validateImpreciseUTCTime = validateImpreciseLocalTime . utcToLocalTime utc

mkImpreciseLocalTime :: LocalTime -> LocalTime
mkImpreciseLocalTime lt = lt {localTimeOfDay = mkImpreciseTimeOfDay (localTimeOfDay lt)}

validateImpreciseLocalTime :: LocalTime -> Validation
validateImpreciseLocalTime lt =
  let tod = localTimeOfDay lt
   in validateImpreciseTimeOfDay tod

mkImpreciseTimeOfDay :: TimeOfDay -> TimeOfDay
mkImpreciseTimeOfDay tod = tod {todSec = fromIntegral (floor (todSec tod) :: Word)}

validateImpreciseTimeOfDay :: TimeOfDay -> Validation
validateImpreciseTimeOfDay tod =
  declare "The number of seconds is integer" $
    let sec = todSec tod
     in ceiling sec == (floor sec :: Int)

instance Validity TZ where
  validate = trivialValidation

instance Validity TZLabel

instance HasCodec TZLabel where
  codec =
    named "TZLabel" $
      bimapCodec parseTZLabel renderTZLabel codec
        <??> docs
    where
      docs = "All possible timezone labels:" : map renderTZLabel [minBound :: TZLabel .. maxBound]

parseTZLabel :: Text -> Either String TZLabel
parseTZLabel t = case fromTZName (TE.encodeUtf8 t) of
  Nothing -> Left $ "Unknown TZ Label: " <> show t
  Just l -> Right l

renderTZLabel :: TZLabel -> Text
renderTZLabel = TE.decodeLatin1 . toTZName
