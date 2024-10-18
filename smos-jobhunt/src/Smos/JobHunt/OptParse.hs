{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.JobHunt.OptParse
  ( Instructions (..),
    Dispatch (..),
    InitSettings (..),
    SendEmailSettings (..),
    Settings (..),
    getInstructions,
  )
where

import Control.Monad.Logger
import Data.Maybe
import Data.Password.Bcrypt
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Network.Socket
import OptEnvConf
import Path
import Path.IO
import Paths_smos_jobhunt
import Smos.CLI.OptParse
import Smos.Data
import Smos.Directory.Resolution
import Smos.Report.Time

getInstructions :: IO Instructions
getInstructions = runSettingsParser version "Smos' jobhunt tool"

data Instructions
  = Instructions
      !Dispatch
      !Settings

instance HasParser Instructions where
  settingsParser =
    withSmosConfig $
      Instructions
        <$> settingsParser
        <*> settingsParser

data Dispatch
  = DispatchInit !InitSettings
  | DispatchSendEmail !SendEmailSettings

instance HasParser Dispatch where
  settingsParser =
    commands
      [ command "init" "Initiate an application process" $
          DispatchInit <$> settingsParser,
        command "email" "Initiate an application process by sending an email" $
          DispatchSendEmail <$> settingsParser
      ]

data InitSettings = InitSettings
  { initSettingCompany :: !Text,
    initSettingContactName :: !(Maybe Text),
    initSettingContactEmail :: !(Maybe Text),
    initSettingUrl :: !(Maybe Text)
  }

instance HasParser InitSettings where
  settingsParser = parseInitSettings

{-# ANN parseInitSettings ("NOCOVER" :: String) #-}
parseInitSettings :: OptEnvConf.Parser InitSettings
parseInitSettings = do
  initSettingCompany <-
    setting
      [ help "The company you just applied to",
        argument,
        reader str,
        metavar "COMPANY"
      ]
  initSettingContactName <-
    optional $
      setting
        [ help "The name of your contact at the company",
          reader str,
          option,
          long "name",
          short 'n',
          metavar "NAME"
        ]
  initSettingContactEmail <-
    optional $
      setting
        [ help "Email address of the contact",
          reader str,
          option,
          short 'e',
          long "email",
          metavar "EMAIL_ADDRESS"
        ]
  initSettingUrl <-
    optional $
      setting
        [ help "The url of the job ad",
          reader str,
          option,
          short 'u',
          long "url",
          metavar "URL"
        ]
  pure InitSettings {..}

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

instance HasParser SendEmailSettings where
  settingsParser = parseSendEmailSettings

{-# ANN parseSendEmailSettings ("NOCOVER" :: String) #-}
parseSendEmailSettings :: OptEnvConf.Parser SendEmailSettings
parseSendEmailSettings = do
  sendEmailSettingToAddress <-
    setting
      [ help "The email address to send the application email to",
        reader str,
        argument,
        metavar "EMAIL_ADDRESS"
      ]
  let sub = subConfig_ "jobhunt" . subEnv_ "jobhunt"
  sendEmailSettingToName <-
    optional $
      sub $
        setting
          [ help "The name of the 'to' email account holder",
            reader str,
            option,
            long "name",
            short 'n',
            env "NAME",
            metavar "NAME"
          ]
  sendEmailSettingCompany <-
    optional $
      sub $
        setting
          [ help "The name of the company you are applying to",
            reader str,
            option,
            long "company",
            short 'c',
            env "COMPANY",
            metavar "COMPANY"
          ]
  sendEmailSettingUrl <-
    optional $
      sub $
        setting
          [ help "The url of the job ad",
            reader str,
            option,
            long "url",
            short 'u',
            env "URL",
            metavar "URL"
          ]
  let subEmail = sub . subAll "email"
  let subTemplate = subEmail . subAll "template"
  sendEmailSettingSubjectTemplateFile <-
    subTemplate $
      filePathSetting
        [ help "Template for the subject of the email",
          name "subject"
        ]
  sendEmailSettingTextTemplateFile <-
    subTemplate $
      filePathSetting
        [ help "Template for the text version of the email",
          name "text"
        ]
  sendEmailSettingHTMLTemplateFile <-
    subTemplate $
      filePathSetting
        [ help "Template for the HTML version of the email",
          name "html"
        ]
  let subFrom = subEmail . subAll "from"
  sendEmailSettingFromName <-
    subFrom $
      optional $
        setting
          [ help "From name",
            reader str,
            name "name",
            metavar "NAME"
          ]
  sendEmailSettingFromAddress <-
    subFrom $
      setting
        [ help "From address",
          reader str,
          name "address",
          metavar "EMAIL_ADDRESS"
        ]
  let subSmtp = subEmail . subAll "smtp"
  sendEmailSettingSMTPServer <-
    subSmtp $
      setting
        [ help "SMTP server domain",
          reader str,
          name "server",
          metavar "DOMAIN"
        ]
  sendEmailSettingSMTPPort <-
    subSmtp $
      optional $
        (fromIntegral :: Word16 -> PortNumber)
          <$> setting
            [ help "SMTP server port",
              reader auto,
              name "port",
              metavar "PORT"
            ]
  sendEmailSettingSMTPUsername <-
    subSmtp $
      setting
        [ help "SMTP server username",
          reader str,
          name "username",
          metavar "USERNAME"
        ]
  sendEmailSettingSMTPPassword <-
    subSmtp $
      mkPassword
        <$> choice
          [ mapIO readSecretTextFile $
              filePathSetting
                [ help "SMTP server password file",
                  name "password-file"
                ],
            setting
              [ help "SMTP server password",
                reader str,
                name "password",
                metavar "PASSWORD"
              ]
          ]
  pure SendEmailSettings {..}

data Settings = Settings
  { setLogLevel :: !LogLevel,
    setJobHuntDirectory :: !(Path Abs Dir),
    setGoal :: !(Maybe PropertyValue),
    setWaitingThreshold :: !(Maybe Time)
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: OptEnvConf.Parser Settings
parseSettings = do
  let sub = subConfig_ "jobhunt" . subEnv_ "jobhunt"
  setJobHuntDirectory <-
    mapIO
      ( \(ds, jhd) -> do
          pd <- resolveDirProjectsDir ds
          resolveDir pd $ fromMaybe "jobhunt" jhd
      )
      $ (,)
        <$> settingsParser
        <*> optional
          ( sub $
              setting
                [ help "The directory to put jobhunt projects in, relative to the projects dir",
                  reader str,
                  name "directory",
                  short 'd',
                  metavar "DIRECTORY_PATH"
                ]
          )
  setLogLevel <- sub settingsParser
  setGoal <-
    optional $
      setting
        [ help "The goal for initialised projects",
          reader $ eitherReader $ parsePropertyValue . T.pack,
          name "goal",
          short 'g',
          metavar "PROPERTY_VALUE"
        ]
  setWaitingThreshold <-
    optional $
      sub $
        setting
          [ help "The waiting threshold initialised projects",
            reader $ eitherReader $ parseTime . T.pack,
            name "waiting-threshhold",
            short 'w',
            metavar "TIME"
          ]
  pure Settings {..}
