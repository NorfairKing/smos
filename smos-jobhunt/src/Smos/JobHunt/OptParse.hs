{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.JobHunt.OptParse
  ( module Smos.JobHunt.OptParse,
    module Smos.JobHunt.OptParse.Types,
  )
where

import Control.Arrow (left)
import Data.Maybe
import qualified Data.Text as T
import Data.Version
import qualified Env
import Options.Applicative
import Options.Applicative.Help.Pretty as Doc
import Path.IO
import Paths_smos_jobhunt
import Smos.CLI.Logging
import Smos.CLI.OptParse as CLI
import Smos.CLI.Password
import Smos.Data
import Smos.Directory.OptParse
import Smos.Directory.Resolution
import Smos.JobHunt.OptParse.Types
import Smos.Report.Time
import qualified System.Environment as System
import System.Exit

getInstructions :: IO Instructions
getInstructions = do
  (Arguments cmd flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions cmd (flagWithRestFlags flags) (envWithRestEnv env) config

combineToInstructions :: Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions cmd Flags {..} Environment {..} mc = do
  let mC :: (Configuration -> Maybe a) -> Maybe a
      mC = (mc >>=)
  let jhMC :: (JobHuntConfiguration -> Maybe a) -> Maybe a
      jhMC = (mC confJobHuntConfiguration >>=)
  let setLogLevel = combineLogLevelSettings flagLogLevel envLogLevel (jhMC jobHuntConfLogLevel)
  setDirectorySettings <-
    combineToDirectorySettings
      defaultDirectorySettings
      flagDirectoryFlags
      envDirectoryEnvironment
      (confDirectoryConfiguration <$> mc)
  pd <- resolveDirProjectsDir setDirectorySettings
  setJobHuntDirectory <-
    resolveDir pd $
      fromMaybe "jobhunt" $
        flagJobHuntDirectory <|> envJobHuntDirectory <|> jhMC jobHuntConfJobHuntDirectory
  let setGoal = flagGoal <|> envGoal <|> jhMC jobHuntConfGoal
  let setWaitingThreshold = flagWaitingThreshold <|> envWaitingThreshold <|> jhMC jobHuntConfWaitingThreshold
  d <- case cmd of
    CommandInit InitFlags {..} -> do
      let initSettingCompany = initFlagCompany
      let initSettingContactName = initFlagContactName
      let initSettingContactEmail = initFlagContactEmail
      let initSettingUrl = initFlagUrl
      pure $ DispatchInit InitSettings {..}
    CommandSendEmail SendEmailFlags {..} -> do
      let SendEmailEnvironment {..} = envSendEmailEnvironment
      let sendEmailSettingToAddress = sendEmailFlagToAddress
      let sendEmailSettingToName = sendEmailFlagToName
      let sendEmailSettingCompany = sendEmailFlagCompany
      let sendEmailSettingUrl = sendEmailFlagUrl

      let EmailFlags {..} = sendEmailFlagEmailFlags
      let EmailEnvironment {..} = sendEmailEnvEmailEnvironment
      let seMC :: (SendEmailConfiguration -> Maybe a) -> Maybe a
          seMC = (jhMC jobHuntConfSendEmailConfiguration >>=)
      let eMC :: (EmailConfiguration -> Maybe a) -> Maybe a
          eMC = (mC confEmailConfiguration >>=)

      sendEmailSettingSubjectTemplateFile <- case sendEmailSubjectTemplateFile <|> sendEmailEnvSubjectTemplateFile <|> seMC sendEmailConfSubjectTemplateFile of
        Nothing -> die "No subject template file configured."
        Just f -> resolveFile' f
      sendEmailSettingTextTemplateFile <- case sendEmailTextTemplateFile <|> sendEmailEnvTextTemplateFile <|> seMC sendEmailConfTextTemplateFile of
        Nothing -> die "No text template file configured."
        Just f -> resolveFile' f
      sendEmailSettingHTMLTemplateFile <- case sendEmailHTMLTemplateFile <|> sendEmailEnvTextTemplateFile <|> seMC sendEmailConfHTMLTemplateFile of
        Nothing -> die "No HTML template file configured."
        Just f -> resolveFile' f

      let sendEmailSettingFromName = emailFlagFromName <|> emailEnvFromName <|> eMC emailConfFromName

      sendEmailSettingFromAddress <- case emailFlagFromAddress <|> emailEnvFromAddress <|> eMC emailConfFromAddress of
        Nothing -> die "No from address configured."
        Just f -> pure f

      let SMTPFlags {..} = emailFlagSMTPFlags
      let SMTPEnvironment {..} = emailEnvSMTPEnvironment
      let smtpMC :: (SMTPConfiguration -> Maybe a) -> Maybe a
          smtpMC = (eMC emailConfSMTPConfiguration >>=)

      sendEmailSettingSMTPServer <- case smtpFlagServer <|> smtpEnvServer <|> smtpMC smtpConfServer of
        Nothing -> die "No SMTP server configured."
        Just s -> pure s

      let sendEmailSettingSMTPPort = smtpFlagPort <|> smtpEnvPort <|> smtpMC smtpConfPort

      sendEmailSettingSMTPUsername <- case smtpFlagUsername <|> smtpEnvUsername <|> smtpMC smtpConfUsername of
        Nothing -> die "No SMTP username configured."
        Just s -> pure s

      mPassword <-
        combinePasswordSettingsWithLogLevel
          setLogLevel
          smtpFlagPassword
          smtpFlagPasswordFile
          smtpEnvPassword
          smtpEnvPasswordFile
          (smtpMC smtpConfPassword)
          (smtpMC smtpConfPasswordFile)
      sendEmailSettingSMTPPassword <- case mPassword of
        Nothing -> die "No SMTP Password configured."
        Just p -> pure p

      pure $ DispatchSendEmail SendEmailSettings {..}
  pure (Instructions d Settings {..})

getEnvironment :: IO (EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
environmentParser =
  Env.prefixed "JOBHUNT_" $
    envWithConfigFileParser $
      Environment
        <$> optional (Env.var logLevelEnvParser "LOG_LEVEL" (Env.help "The minimal severity of log messages"))
        <*> directoryEnvironmentParser
        <*> optional (Env.var Env.str "DIRECTORY" (Env.help "Text version of the email template file"))
        <*> optional (Env.var (left Env.UnreadError . parsePropertyValue . T.pack) "GOAL" (Env.help "The goal for initiated projects"))
        <*> optional (Env.var (left Env.UnreadError . parseTime . T.pack) "WATIING_THRESHOLD" (Env.help "The waiting threshold for initiated projects"))
        <*> sendEmailEnvironmentParser

prefixedSendEmailEnvironmentParser :: Env.Parser Env.Error SendEmailEnvironment
prefixedSendEmailEnvironmentParser = Env.prefixed "SMOS_JOBHUNT_" sendEmailEnvironmentParser

sendEmailEnvironmentParser :: Env.Parser Env.Error SendEmailEnvironment
sendEmailEnvironmentParser =
  SendEmailEnvironment
    <$> optional (Env.var Env.str "EMAIL_SUBJECT_TEMPLATE" (Env.help "Subject of the email template file"))
    <*> optional (Env.var Env.str "EMAIL_TEXT_TEMPLATE" (Env.help "Text version of the email template file"))
    <*> optional (Env.var Env.str "EMAIL_HTML_TEMPLATE" (Env.help "HTML version of the email template file"))
    <*> emailEnvironmentParser

emailEnvironmentParser :: Env.Parser Env.Error EmailEnvironment
emailEnvironmentParser =
  Env.prefixed "EMAIL_" $
    EmailEnvironment
      <$> optional (Env.var Env.str "FROM_NAME" (Env.help "Email from name"))
      <*> optional (Env.var Env.str "FROM_ADDRESS" (Env.help "Email from address"))
      <*> smtpEnvironmentParser

smtpEnvironmentParser :: Env.Parser Env.Error SMTPEnvironment
smtpEnvironmentParser =
  Env.prefixed "SMTP_" $
    SMTPEnvironment
      <$> optional (Env.var Env.str "SERVER" (Env.help "SMTP Server domain"))
      <*> optional (Env.var Env.auto "PORT" (Env.help "SMTP Server port"))
      <*> optional (Env.var Env.str "USERNAME" (Env.help "SMTP Username"))
      <*> optional (Env.var Env.str "PASSWORD" (Env.help "SMTP Password"))
      <*> optional (Env.var Env.str "PASSWORD_FILE" (Env.help "SMTP Password file"))

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = CLI.execOptionParserPure argumentsParser

argumentsParser :: ParserInfo Arguments
argumentsParser = info (helper <*> parseArguments) help_
  where
    help_ = fullDesc <> progDescDoc (Just description)
    description :: Doc
    description =
      Doc.vsep $
        map Doc.text $
          [ "",
            "Smos JobHunt Tool version: " <> showVersion version,
            ""
          ]
            ++ readWriteDataVersionsHelpMessage

parseArguments :: Parser Arguments
parseArguments = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
  hsubparser $
    mconcat
      [ command "init" (CommandInit <$> parseCommandInit),
        command "email" (CommandSendEmail <$> parseCommandSendEmail)
      ]

parseCommandInit :: ParserInfo InitFlags
parseCommandInit = info parser modifier
  where
    modifier = fullDesc <> progDesc "Initiate an application process"
    parser =
      InitFlags
        <$> strArgument
          ( mconcat
              [ help "The company you just applied to",
                metavar "COMPANY"
              ]
          )
        <*> optional
          ( strOption
              ( mconcat
                  [ short 'n',
                    long "name",
                    metavar "NAME",
                    help "The name of your contact at the company"
                  ]
              )
          )
        <*> optional
          ( strOption
              ( mconcat
                  [ short 'e',
                    long "email",
                    metavar "EMAIL_ADDRESS",
                    help "Email address of the contact"
                  ]
              )
          )
        <*> optional
          ( strOption
              ( mconcat
                  [ short 'u',
                    long "url",
                    metavar "URL",
                    help "The url of the job ad"
                  ]
              )
          )

parseCommandSendEmail :: ParserInfo SendEmailFlags
parseCommandSendEmail = info parser modifier
  where
    modifier = fullDesc <> progDesc "Initiate an application process by sending an email"
    parser =
      SendEmailFlags
        <$> strArgument
          ( mconcat
              [ metavar "EMAIL_ADDRESS",
                help "The email address to send the application email to"
              ]
          )
        <*> optional
          ( strOption
              ( mconcat
                  [ short 'n',
                    long "name",
                    metavar "NAME",
                    help "The name of the 'to' email account holder"
                  ]
              )
          )
        <*> optional
          ( strOption
              ( mconcat
                  [ short 'c',
                    long "company",
                    metavar "COMPANY",
                    help "The name of the company you are applying to"
                  ]
              )
          )
        <*> optional
          ( strOption
              ( mconcat
                  [ short 'u',
                    long "url",
                    metavar "URL",
                    help "The url of the job ad"
                  ]
              )
          )
        <*> optional
          ( strOption
              ( mconcat
                  [ long "email-subject-template",
                    help "Template for the subject of the email",
                    metavar "FILEPATH",
                    completer $ bashCompleter "file"
                  ]
              )
          )
        <*> optional
          ( strOption
              ( mconcat
                  [ long "email-text-template",
                    help "Template for the text version of the email",
                    metavar "FILEPATH",
                    completer $ bashCompleter "file"
                  ]
              )
          )
        <*> optional
          ( strOption
              ( mconcat
                  [ long "email-html-template",
                    help "Template for the HTML version of the email",
                    metavar "FILEPATH",
                    completer $ bashCompleter "file"
                  ]
              )
          )
        <*> parseEmailFlags

parseEmailFlags :: Parser EmailFlags
parseEmailFlags =
  EmailFlags
    <$> optional
      ( strOption
          ( mconcat
              [ long "email-from-name",
                metavar "NAME",
                help "From name"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "email-from-address",
                metavar "EMAIL_ADDRESS",
                help "From address"
              ]
          )
      )
    <*> parseSMTPFlags

parseSMTPFlags :: Parser SMTPFlags
parseSMTPFlags =
  SMTPFlags
    <$> optional
      ( strOption
          ( mconcat
              [ long "smtp-server",
                metavar "DOMAIN",
                help "SMTP server domain"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "smtp-port",
                metavar "PORT",
                help "SMTP server port"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "smtp-username",
                metavar "USERNAME",
                help "SMTP server username"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "smtp-password",
                metavar "PASSWORD",
                help "SMTP server password"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "smtp-password-file",
                metavar "FILEPATH",
                help "SMTP server password file",
                completer $ bashCompleter "file"
              ]
          )
      )

parseFlags :: Parser (FlagsWithConfigFile Flags)
parseFlags =
  parseFlagsWithConfigFile $
    Flags
      <$> parseLogLevelOption
      <*> parseDirectoryFlags
      <*> optional
        ( strOption
            ( mconcat
                [ short 'd',
                  long "directory",
                  metavar "DIRECTORY",
                  help "The directory to put jobhunt projects in"
                ]
            )
        )
      <*> optional
        ( strOption
            ( mconcat
                [ short 'g',
                  long "goal",
                  metavar "GOAL",
                  help "The goal for initialised projects"
                ]
            )
        )
      <*> optional
        ( option
            (eitherReader (parseTime . T.pack))
            ( mconcat
                [ short 'w',
                  long "waiting-threshhold",
                  metavar "TIME",
                  help "The waiting threshold initialised projects"
                ]
            )
        )
