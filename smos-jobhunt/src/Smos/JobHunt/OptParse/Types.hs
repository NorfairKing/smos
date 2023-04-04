{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.JobHunt.OptParse.Types where

import Autodocodec
import Control.Monad.Logger
import Data.Text (Text)
import Data.Word
import Network.Socket
import Path
import Smos.CLI.Logging ()
import Smos.CLI.OptParse
import Smos.CLI.Password
import Smos.Data
import Smos.Directory.OptParse.Types
import Smos.Report.Time

data Arguments
  = Arguments
      !Command
      !(FlagsWithConfigFile Flags)
  deriving (Show)

data Command
  = CommandInit !InitFlags
  | CommandSendEmail !SendEmailFlags
  deriving (Show)

data InitFlags = InitFlags
  { initFlagCompany :: !Text,
    initFlagContactName :: !(Maybe Text),
    initFlagContactEmail :: !(Maybe Text),
    initFlagUrl :: !(Maybe Text)
  }
  deriving (Show, Eq)

data SendEmailFlags = SendEmailFlags
  { sendEmailFlagToAddress :: !Text,
    sendEmailFlagToName :: !(Maybe Text),
    sendEmailFlagCompany :: !(Maybe Text),
    sendEmailFlagUrl :: !(Maybe Text),
    sendEmailSubjectTemplateFile :: !(Maybe FilePath),
    sendEmailTextTemplateFile :: !(Maybe FilePath),
    sendEmailHTMLTemplateFile :: !(Maybe FilePath),
    sendEmailFlagEmailFlags :: !EmailFlags
  }
  deriving (Show)

data EmailFlags = EmailFlags
  { emailFlagFromName :: !(Maybe Text),
    emailFlagFromAddress :: !(Maybe Text),
    emailFlagSMTPFlags :: !SMTPFlags
  }
  deriving (Show)

data SMTPFlags = SMTPFlags
  { smtpFlagServer :: !(Maybe Text),
    smtpFlagPort :: !(Maybe PortNumber),
    smtpFlagUsername :: !(Maybe Text),
    smtpFlagPassword :: !(Maybe Password),
    smtpFlagPasswordFile :: !(Maybe FilePath)
  }
  deriving (Show)

data Flags = Flags
  { flagLogLevel :: !(Maybe LogLevel),
    flagDirectoryFlags :: !DirectoryFlags,
    flagJobHuntDirectory :: !(Maybe FilePath),
    flagGoal :: !(Maybe PropertyValue),
    flagWaitingThreshold :: !(Maybe Time)
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confDirectoryConfiguration :: !DirectoryConfiguration,
    confEmailConfiguration :: !(Maybe EmailConfiguration),
    confJobHuntConfiguration :: !(Maybe JobHuntConfiguration)
  }
  deriving (Show)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> objectCodec .= confDirectoryConfiguration
        <*> optionalFieldOrNull emailConfigurationKey "The email configuration" .= confEmailConfiguration
        <*> optionalFieldOrNull "jobhunt" "The jobhunt tool configuration" .= confJobHuntConfiguration

data JobHuntConfiguration = JobHuntConfiguration
  { jobHuntConfLogLevel :: !(Maybe LogLevel),
    jobHuntConfJobHuntDirectory :: !(Maybe FilePath),
    jobHuntConfGoal :: !(Maybe PropertyValue),
    jobHuntConfWaitingThreshold :: !(Maybe Time),
    jobHuntConfSendEmailConfiguration :: !(Maybe SendEmailConfiguration)
  }
  deriving (Show, Eq)

instance HasCodec JobHuntConfiguration where
  codec =
    object "JobHuntConfiguration" $
      JobHuntConfiguration
        <$> optionalFieldOrNull "log-level" "Minimal severity of log messages" .= jobHuntConfLogLevel
        <*> optionalFieldOrNull "directory" "Directory, relative to the projects dir, or absolute" .= jobHuntConfJobHuntDirectory
        <*> optionalFieldOrNull "goal" "The goal property for initialised projects" .= jobHuntConfGoal
        <*> optionalFieldOrNull "waiting-threshold" "The waiting threshold for initialised projects" .= jobHuntConfWaitingThreshold
        <*> optionalFieldOrNull "email" "The configuration for the email command" .= jobHuntConfSendEmailConfiguration

data SendEmailConfiguration = SendEmailConfiguration
  { sendEmailConfSubjectTemplateFile :: !(Maybe FilePath),
    sendEmailConfTextTemplateFile :: !(Maybe FilePath),
    sendEmailConfHTMLTemplateFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq)

instance HasCodec SendEmailConfiguration where
  codec =
    object "SendEmailConfiguration " $
      SendEmailConfiguration
        <$> optionalFieldOrNull "email-subject-template" "Template file for the subject of the email" .= sendEmailConfSubjectTemplateFile
        <*> optionalFieldOrNull "email-text-template" "Template file for the text version of the email" .= sendEmailConfTextTemplateFile
        <*> optionalFieldOrNull "email-html-template" "Template file for the text version of the email" .= sendEmailConfHTMLTemplateFile

emailConfigurationKey :: Text
emailConfigurationKey = "email"

data EmailConfiguration = EmailConfiguration
  { emailConfFromName :: !(Maybe Text),
    emailConfFromAddress :: !(Maybe Text),
    emailConfSMTPConfiguration :: !(Maybe SMTPConfiguration)
  }
  deriving (Show)

instance HasCodec EmailConfiguration where
  codec =
    object "EmailConfiguration" $
      EmailConfiguration
        <$> optionalFieldOrNull "from-name" "The from name for sending email" .= emailConfFromName
        <*> optionalFieldOrNull "from-address" "The from address for sending email" .= emailConfFromAddress
        <*> optionalFieldOrNull "smtp" "The from address for sending email" .= emailConfSMTPConfiguration

data SMTPConfiguration = SMTPConfiguration
  { smtpConfServer :: !(Maybe Text),
    smtpConfPort :: !(Maybe PortNumber),
    smtpConfUsername :: !(Maybe Text),
    smtpConfPassword :: !(Maybe Password),
    smtpConfPasswordFile :: !(Maybe FilePath)
  }
  deriving (Show)

instance HasCodec SMTPConfiguration where
  codec =
    object "SMTPConfiguration" $
      SMTPConfiguration
        <$> requiredField "server" "The smtp server" .= smtpConfServer
        <*> optionalFieldOrNullWith "port" (dimapCodec (fromIntegral :: Word16 -> PortNumber) fromIntegral codec) "The smtp server port" .= smtpConfPort
        <*> optionalFieldOrNull "username" "The username to authenticate with the smtp server" .= smtpConfUsername
        <*> optionalFieldOrNullWith "password" (dimapCodec mkPassword unsafeShowPassword codec) "The password to authenticate with the smtp server" .= smtpConfPassword
        <*> optionalFieldOrNull "password-file" "Path to a file with the password to authenticate with the smtp server" .= smtpConfPasswordFile

data Environment = Environment
  { envLogLevel :: !(Maybe LogLevel),
    envDirectoryEnvironment :: !DirectoryEnvironment,
    envJobHuntDirectory :: !(Maybe FilePath),
    envGoal :: !(Maybe PropertyValue),
    envWaitingThreshold :: !(Maybe Time),
    envSendEmailEnvironment :: !SendEmailEnvironment
  }
  deriving (Show)

data SendEmailEnvironment = SendEmailEnvironment
  { sendEmailEnvSubjectTemplateFile :: !(Maybe FilePath),
    sendEmailEnvTextTemplateFile :: !(Maybe FilePath),
    sendEmailEnvHTMLTemplateFile :: !(Maybe FilePath),
    sendEmailEnvEmailEnvironment :: !EmailEnvironment
  }
  deriving (Show)

data EmailEnvironment = EmailEnvironment
  { emailEnvFromName :: !(Maybe Text),
    emailEnvFromAddress :: !(Maybe Text),
    emailEnvSMTPEnvironment :: !SMTPEnvironment
  }
  deriving (Show)

data SMTPEnvironment = SMTPEnvironment
  { smtpEnvServer :: !(Maybe Text),
    smtpEnvPort :: !(Maybe PortNumber),
    smtpEnvUsername :: !(Maybe Text),
    smtpEnvPassword :: !(Maybe Password),
    smtpEnvPasswordFile :: !(Maybe FilePath)
  }
  deriving (Show)

data Instructions
  = Instructions
      !Dispatch
      !Settings
  deriving (Show)

data Dispatch
  = DispatchInit !InitSettings
  | DispatchSendEmail !SendEmailSettings
  deriving (Show)

data InitSettings = InitSettings
  { initSettingCompany :: !Text,
    initSettingContactName :: !(Maybe Text),
    initSettingContactEmail :: !(Maybe Text),
    initSettingUrl :: !(Maybe Text)
  }
  deriving (Show, Eq)

data SendEmailSettings = SendEmailSettings
  { sendEmailSettingToAddress :: !Text,
    sendEmailSettingToName :: !(Maybe Text),
    sendEmailSettingCompany :: !(Maybe Text),
    sendEmailSettingUrl :: !(Maybe Text),
    sendEmailSettingSubjectTemplateFile :: !(Path Abs File),
    sendEmailSettingTextTemplateFile :: !(Path Abs File),
    sendEmailSettingHTMLTemplateFile :: !(Path Abs File),
    sendEmailSettingFromName :: !(Maybe Text),
    sendEmailSettingFromAddress :: !Text,
    sendEmailSettingSMTPServer :: !Text,
    sendEmailSettingSMTPPort :: !(Maybe PortNumber),
    sendEmailSettingSMTPUsername :: !Text,
    sendEmailSettingSMTPPassword :: !Password
  }
  deriving (Show)

data Settings = Settings
  { setLogLevel :: !LogLevel,
    setJobHuntDirectory :: !(Path Abs Dir),
    setGoal :: !(Maybe PropertyValue),
    setWaitingThreshold :: !(Maybe Time)
  }
  deriving (Show, Eq)
