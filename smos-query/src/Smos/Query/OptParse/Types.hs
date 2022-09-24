{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Query.OptParse.Types
  ( module Smos.Report.Clock.Types,
    module Smos.Report.Agenda.Types,
    module Smos.Query.OptParse.Types,
    module Smos.Report.ShouldPrint,
  )
where

import Autodocodec
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import Smos.Data
import Smos.Report.Agenda.Types
import Smos.Report.Archive
import Smos.Report.Clock.Types
import Smos.Report.Config
import Smos.Report.Filter
import qualified Smos.Report.OptParse.Types as Report
import Smos.Report.Period
import Smos.Report.Projection
import Smos.Report.Report
import Smos.Report.ShouldPrint
import Smos.Report.Sorter
import Smos.Report.Time
import Smos.Report.TimeBlock
import Text.Colour
import Text.Colour.Code
import Text.Colour.Layout

data Arguments
  = Arguments Command (Report.FlagsWithConfigFile Flags)
  deriving (Show, Eq)

data Instructions
  = Instructions Dispatch Settings

data Command
  = CommandEntry !EntryFlags
  | CommandReport !ReportFlags
  | CommandWaiting !WaitingFlags
  | CommandNext !NextFlags
  | CommandClock !ClockFlags
  | CommandAgenda !AgendaFlags
  | CommandProjects !ProjectsFlags
  | CommandStuck !StuckFlags
  | CommandWork !WorkFlags
  | CommandLog !LogFlags
  | CommandStats !StatsFlags
  | CommandTags !TagsFlags
  deriving (Show, Eq)

data EntryFlags = EntryFlags
  { entryFlagFilter :: !(Maybe EntryFilter),
    entryFlagProjection :: !(Maybe (NonEmpty Projection)),
    entryFlagSorter :: !(Maybe Sorter),
    entryFlagHideArchive :: !(Maybe HideArchive),
    entryFlagOutputFormat :: !(Maybe OutputFormat)
  }
  deriving (Show, Eq)

data ReportFlags = ReportFlags
  { reportFlagReportName :: !(Maybe Text),
    reportFlagOutputFormat :: !(Maybe OutputFormat)
  }
  deriving (Show, Eq)

data WorkFlags = WorkFlags
  { workFlagContext :: !(Maybe ContextName),
    workFlagTime :: !(Maybe Time),
    workFlagFilter :: !(Maybe EntryFilter),
    workFlagProjection :: !(Maybe (NonEmpty Projection)),
    workFlagSorter :: !(Maybe Sorter),
    workFlagHideArchive :: !(Maybe HideArchive),
    workFlagWaitingThreshold :: !(Maybe Time),
    workFlagStuckThreshold :: !(Maybe Time)
  }
  deriving (Show, Eq)

data WaitingFlags = WaitingFlags
  { waitingFlagFilter :: !(Maybe EntryFilter),
    waitingFlagHideArchive :: !(Maybe HideArchive),
    waitingFlagThreshold :: !(Maybe Time)
  }
  deriving (Show, Eq)

data NextFlags = NextFlags
  { nextFlagFilter :: !(Maybe EntryFilter),
    nextFlagHideArchive :: !(Maybe HideArchive)
  }
  deriving (Show, Eq)

data ClockFlags = ClockFlags
  { clockFlagFilter :: !(Maybe EntryFilter),
    clockFlagPeriodFlags :: !(Maybe Period),
    clockFlagBlockFlags :: !(Maybe TimeBlock),
    clockFlagOutputFormat :: !(Maybe OutputFormat),
    clockFlagClockFormat :: !(Maybe ClockFormatFlags),
    clockFlagReportStyle :: !(Maybe ClockReportStyle),
    clockFlagHideArchive :: !(Maybe HideArchive)
  }
  deriving (Show, Eq)

data ClockFormatFlags
  = ClockFormatTemporalFlag !(Maybe TemporalClockResolution)
  | ClockFormatDecimalFlag !(Maybe DecimalClockResolution)
  deriving (Show, Eq)

data AgendaFlags = AgendaFlags
  { agendaFlagFilter :: !(Maybe EntryFilter),
    agendaFlagHistoricity :: !(Maybe AgendaHistoricity),
    agendaFlagBlock :: !(Maybe TimeBlock),
    agendaFlagHideArchive :: !(Maybe HideArchive),
    agendaFlagPeriod :: !(Maybe Period)
  }
  deriving (Show, Eq)

data ProjectsFlags = ProjectsFlags
  { projectsFlagFilter :: !(Maybe ProjectFilter)
  }
  deriving (Show, Eq)

data StuckFlags = StuckFlags
  { stuckFlagFilter :: !(Maybe ProjectFilter),
    stuckFlagThreshold :: !(Maybe Time)
  }
  deriving (Show, Eq)

data LogFlags = LogFlags
  { logFlagFilter :: !(Maybe EntryFilter),
    logFlagPeriodFlags :: !(Maybe Period),
    logFlagBlockFlags :: !(Maybe TimeBlock),
    logFlagHideArchive :: !(Maybe HideArchive)
  }
  deriving (Show, Eq, Generic)

newtype StatsFlags = StatsFlags
  { statsFlagPeriodFlags :: Maybe Period
  }
  deriving (Show, Eq, Generic)

newtype TagsFlags = TagsFlags
  { tagsFlagFilter :: Maybe EntryFilter
  }
  deriving (Show, Eq, Generic)

newtype Flags = Flags
  { flagReportFlags :: Report.Flags
  }
  deriving (Show, Eq, Generic)

emptyEnvironment :: Environment
emptyEnvironment =
  Environment
    { envReportEnvironment = Report.emptyEnvironment,
      envHideArchive = Nothing
    }

data Environment = Environment
  { envReportEnvironment :: !Report.Environment,
    envHideArchive :: !(Maybe HideArchive)
  }
  deriving (Show, Eq, Generic)

defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration
    { confReportConf = Report.defaultConfiguration,
      confHideArchive = Nothing,
      confPreparedReportConfiguration = Nothing,
      confColourConfiguration = Nothing
    }

data Configuration = Configuration
  { confReportConf :: !Report.Configuration,
    confHideArchive :: !(Maybe HideArchive),
    confPreparedReportConfiguration :: !(Maybe PreparedReportConfiguration),
    confColourConfiguration :: !(Maybe ColourConfiguration)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

instance Validity Configuration

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> objectCodec .= confReportConf
        <*> optionalFieldOrNull "hide-archive" "Whether or not to consider the archive, by default" .= confHideArchive
        <*> optionalFieldOrNull preparedReportConfigurationKey "Prepared report config" .= confPreparedReportConfiguration
        <*> optionalFieldOrNull colourConfigurationKey "Colour config" .= confColourConfiguration

preparedReportConfigurationKey :: Text
preparedReportConfigurationKey = "report"

colourConfigurationKey :: Text
colourConfigurationKey = "colour"

data PreparedReportConfiguration = PreparedReportConfiguration
  { preparedReportConfAvailableReports :: !(Maybe (Map Text PreparedReport))
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec PreparedReportConfiguration)

instance Validity PreparedReportConfiguration

instance HasCodec PreparedReportConfiguration where
  codec =
    object "PreparedReportConfiguration" $
      PreparedReportConfiguration <$> optionalFieldOrNull "reports" "Custom reports" .= preparedReportConfAvailableReports

data ColourConfiguration = ColourConfiguration
  { -- | How to background-colour tables
    --
    -- The first maybe is for whether this is defined in the configuration file.
    -- The second maybe is for whether any background colour should be used.
    colourConfigurationBackground :: !(Maybe TableBackgroundConfiguration)
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ColourConfiguration)

instance Validity ColourConfiguration

instance HasCodec ColourConfiguration where
  codec =
    object "ColourConfiguration" $
      ColourConfiguration
        <$> optionalFieldOrNull "background" "The table background colours" .= colourConfigurationBackground

data TableBackgroundConfiguration
  = UseTableBackground !TableBackground
  | NoTableBackground
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TableBackgroundConfiguration)

instance Validity TableBackgroundConfiguration

instance HasCodec TableBackgroundConfiguration where
  codec = dimapCodec f g $ eitherCodec nullCodec codec
    where
      f = \case
        Left () -> NoTableBackground
        Right tb -> UseTableBackground tb
      g = \case
        NoTableBackground -> Left ()
        UseTableBackground tb -> Right tb

instance HasCodec TableBackground where
  codec =
    dimapCodec f g $
      eitherCodec
        (codec <?> "A single background colour")
        ( object "Bicolour" $
            (,)
              <$> optionalFieldOrNull "even" "background for even-numbered table-rows (0-indexed)" .= fst
              <*> optionalFieldOrNull "odd" "background for odd-numbered table-rows" .= snd
        )
    where
      f = \case
        Left c -> SingleColour c
        Right (e, o) -> Bicolour e o
      g = \case
        SingleColour c -> Left c
        Bicolour e o -> Right (e, o)

instance FromJSON TableBackground where
  parseJSON = parseJSONViaCodec

instance ToJSON TableBackground where
  toJSON = toJSONViaCodec
  toEncoding = toEncodingViaCodec

instance HasCodec Colour where
  codec =
    named "Colour" $
      dimapCodec from to $ eitherCodec colour8Codec $ eitherCodec colour8BitCodec colour24BitCodec
    where
      from = \case
        Left (i, tc) -> Colour8 i tc
        Right (Left w) -> Colour8Bit w
        Right (Right (r, g, b)) -> Colour24Bit r g b
      to = \case
        Colour8 i tc -> Left (i, tc)
        Colour8Bit w -> Right (Left w)
        Colour24Bit r g b -> Right (Right (r, g, b))
      colour8Codec =
        bimapCodec
          ( \t -> do
              let colourCase :: String -> Either String TerminalColour
                  colourCase = \case
                    "black" -> Right Black
                    "red" -> Right Red
                    "green" -> Right Green
                    "yellow" -> Right Yellow
                    "blue" -> Right Blue
                    "magenta" -> Right Magenta
                    "cyan" -> Right Cyan
                    "white" -> Right White
                    s -> Left $ "Unknown colour: " <> s
              case words t of
                [colourStr] -> (,) Dull <$> colourCase colourStr
                [intensityStr, colourStr] -> do
                  intensity <- case intensityStr of
                    "bright" -> Right Bright
                    "dull" -> Right Dull
                    _ -> Left $ "Unknown colour intensity: " <> intensityStr
                  (,) intensity <$> colourCase colourStr
                _ -> Left "Specify a terminal colour as two words, e. g. 'dull red' or 'bright blue'."
          )
          ( \(intensity, colour8) ->
              mconcat
                [ case intensity of
                    Dull -> ""
                    Bright -> "bright ",
                  case colour8 of
                    Black -> "black"
                    Red -> "red"
                    Green -> "green"
                    Yellow -> "yellow"
                    Blue -> "blue"
                    Magenta -> "magenta"
                    Cyan -> "cyan"
                    White -> "white"
                ]
          )
          codec
      colour8BitCodec =
        codec
          <??> [ "Set this to a number between 0 and 255 that represents the colour that you want from the 8-bit colour schema.",
                 "See this overview on wikipedia for more information:",
                 "https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit"
               ]
      colour24BitCodec =
        object "Colour24Bit" $
          let r (x, _, _) = x
              g (_, x, _) = x
              b (_, _, x) = x
           in (,,)
                <$> requiredField "red" "The red component, [0..255]" .= r
                <*> requiredField "green" "The green component, [0..255]" .= g
                <*> requiredField "blue" "The blue component, [0..255]" .= b

instance FromJSON Colour where
  parseJSON = parseJSONViaCodec

instance ToJSON Colour where
  toJSON = toJSONViaCodec
  toEncoding = toEncodingViaCodec

data Dispatch
  = DispatchEntry !EntrySettings
  | DispatchReport !ReportSettings
  | DispatchWork !WorkSettings
  | DispatchWaiting !WaitingSettings
  | DispatchNext !NextSettings
  | DispatchClock !ClockSettings
  | DispatchAgenda !AgendaSettings
  | DispatchProjects !ProjectsSettings
  | DispatchStuck !StuckSettings
  | DispatchLog !LogSettings
  | DispatchStats !StatsSettings
  | DispatchTags !TagsSettings
  deriving (Show, Eq, Generic)

data EntrySettings = EntrySettings
  { entrySetFilter :: !(Maybe EntryFilter),
    entrySetProjection :: !(NonEmpty Projection),
    entrySetSorter :: !(Maybe Sorter),
    entrySetHideArchive :: !HideArchive,
    entrySetOutputFormat :: !OutputFormat
  }
  deriving (Show, Eq, Generic)

data ReportSettings = ReportSettings
  { reportSetReportName :: !(Maybe Text),
    reportSetAvailableReports :: !(Map Text PreparedReport),
    reportSetOutputFormat :: !OutputFormat
  }
  deriving (Show, Eq, Generic)

data WorkSettings = WorkSettings
  { workSetContext :: !(Maybe ContextName),
    workSetContexts :: !(Map ContextName EntryFilter),
    workSetChecks :: !(Set EntryFilter),
    workSetTime :: !(Maybe Time),
    workSetTimeProperty :: !(Maybe PropertyName),
    workSetBaseFilter :: !(Maybe EntryFilter),
    workSetFilter :: !(Maybe EntryFilter),
    workSetProjection :: !(NonEmpty Projection),
    workSetSorter :: !(Maybe Sorter),
    workSetHideArchive :: !HideArchive,
    workSetWaitingThreshold :: !Time,
    workSetStuckThreshold :: !Time
  }
  deriving (Show, Eq, Generic)

data WaitingSettings = WaitingSettings
  { waitingSetFilter :: !(Maybe EntryFilter),
    waitingSetHideArchive :: !HideArchive,
    waitingSetThreshold :: !Time
  }
  deriving (Show, Eq, Generic)

data NextSettings = NextSettings
  { nextSetFilter :: !(Maybe EntryFilter),
    nextSetHideArchive :: !HideArchive
  }
  deriving (Show, Eq, Generic)

data ClockSettings = ClockSettings
  { clockSetFilter :: !(Maybe EntryFilter),
    clockSetPeriod :: !Period,
    clockSetBlock :: !TimeBlock,
    clockSetOutputFormat :: !OutputFormat,
    clockSetClockFormat :: !ClockFormat,
    clockSetReportStyle :: !ClockReportStyle,
    clockSetHideArchive :: !HideArchive
  }
  deriving (Show, Eq, Generic)

data AgendaSettings = AgendaSettings
  { agendaSetFilter :: !(Maybe EntryFilter),
    agendaSetHistoricity :: !AgendaHistoricity,
    agendaSetBlock :: !TimeBlock,
    agendaSetHideArchive :: !HideArchive,
    agendaSetPeriod :: !Period
  }
  deriving (Show, Eq, Generic)

data ProjectsSettings = ProjectsSettings
  { projectsSetFilter :: !(Maybe ProjectFilter)
  }
  deriving (Show, Eq, Generic)

data StuckSettings = StuckSettings
  { stuckSetFilter :: !(Maybe ProjectFilter),
    stuckSetThreshold :: !Time
  }
  deriving (Show, Eq, Generic)

data LogSettings = LogSettings
  { logSetFilter :: !(Maybe EntryFilter),
    logSetPeriod :: !Period,
    logSetBlock :: !TimeBlock,
    logSetHideArchive :: !HideArchive
  }
  deriving (Show, Eq, Generic)

data StatsSettings = StatsSettings
  { statsSetPeriod :: !Period
  }
  deriving (Show, Eq, Generic)

data TagsSettings = TagsSettings
  { tagsSetFilter :: !(Maybe EntryFilter)
  }
  deriving (Show, Eq, Generic)

data OutputFormat
  = OutputPretty
  | OutputYaml
  | OutputJSON
  | OutputJSONPretty
  deriving (Show, Eq, Generic)

data Settings = Settings
  { settingDirectoryConfig :: !DirectoryConfig,
    settingColourSettings :: !ColourSettings
  }
  deriving (Show, Eq, Generic)

data ColourSettings = ColourSettings
  { colourSettingBackground :: !TableBackgroundConfiguration
  }
  deriving (Show, Eq, Generic)
