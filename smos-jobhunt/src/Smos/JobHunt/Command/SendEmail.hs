{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.JobHunt.Command.SendEmail (smosJobHuntSendEmail) where

import Control.Monad.Logger
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import Data.Time
import qualified Network.HaskellNet.SMTP as SMTP
import qualified Network.HaskellNet.SMTP.SSL as SMTP
import Network.Mail.Mime
import Path
import Path.IO
import Smos.CLI.Password
import Smos.CLI.Prompt
import Smos.JobHunt.Command.Init
import Smos.JobHunt.OptParse
import System.Exit
import Text.Colour
import Text.Colour.Term
import Text.Mustache
import UnliftIO

smosJobHuntSendEmail :: Settings -> SendEmailSettings -> LoggingT IO ()
smosJobHuntSendEmail settings SendEmailSettings {..} = withSystemTempDir "smos-jobhunt" $ \tdir -> do
  let company = case sendEmailSettingCompany of
        Just c -> c
        Nothing -> snd $ T.breakOnEnd "@" sendEmailSettingToAddress

  let mustacheValue =
        object
          [ "name" .= sendEmailSettingToName,
            "company" .= company,
            "url" .= sendEmailSettingUrl,
            "no_url" .= isNothing sendEmailSettingUrl
          ]

  let renderWithoutWarnings :: Path Abs File -> LoggingT IO Text
      renderWithoutWarnings templateFile = do
        template <- compileMustacheFile (fromAbsFile templateFile) `catch` (liftIO . die . (displayException :: MustacheException -> String))
        let (warnings, result) = renderMustacheW template mustacheValue
        case warnings of
          [] -> pure $ T.strip $ LT.toStrict result
          _ -> liftIO $ die $ unlines $ "Template rendered with warnings:" : map displayMustacheWarning warnings

  subject <- renderWithoutWarnings sendEmailSettingSubjectTemplateFile
  textVersion <- renderWithoutWarnings sendEmailSettingTextTemplateFile
  htmlVersion <- renderWithoutWarnings sendEmailSettingHTMLTemplateFile

  htmlTempFile <- resolveFile tdir "email.html"
  liftIO $ T.writeFile (fromAbsFile htmlTempFile) htmlVersion

  let promptChunks =
        [ [fore blue "Subject:"],
          [fore yellow (chunk subject)],
          [fore blue "Text:"],
          [fore yellow $ chunk textVersion],
          [fore blue "HTML:"],
          [fore yellow $ chunk htmlVersion],
          [ fore blue "You can check the html version in your browser: ",
            fore blue $ chunk (T.pack (fromAbsFile htmlTempFile))
          ]
        ]

  liftIO $ putChunksLocale $ concatMap (<> ["\n"]) promptChunks

  let from =
        Address
          { addressName = sendEmailSettingFromName,
            addressEmail = sendEmailSettingFromAddress
          }
  let to =
        Address
          { addressName = sendEmailSettingToName,
            addressEmail = sendEmailSettingToAddress
          }

  yn <- liftIO $ promptYesNo No $ T.pack $ unwords ["Send the above email to", show sendEmailSettingToAddress, "now?"]
  case yn of
    No -> logWarnN "Not sending any email."
    Yes -> do
      -- Prepare the mail before connecting
      now <- liftIO getCurrentTime
      let mail =
            (emptyMail from)
              { mailTo = [to],
                mailBcc = [from],
                mailHeaders =
                  [ ("Subject", subject),
                    -- Set the date command because mail-tester said so:
                    -- https://stackoverflow.com/questions/50012445/linux-date-command-to-generate-email-header
                    ("Date", T.pack $ formatTime defaultTimeLocale "%a, %-d %b %Y %H:%M:%S %z" now)
                  ],
                mailParts =
                  [ [ plainPart (LT.fromStrict textVersion),
                      htmlPart (LT.fromStrict htmlVersion)
                    ]
                  ]
              }

      let smtpSets = case sendEmailSettingSMTPPort of
            Nothing -> SMTP.defaultSettingsSMTPSSL
            Just port -> SMTP.defaultSettingsSMTPSSL {SMTP.sslPort = port}
      bracket
        (liftIO (SMTP.connectSMTPSSLWithSettings (T.unpack sendEmailSettingSMTPServer) smtpSets))
        (liftIO . SMTP.closeSMTP)
        $ \smtpConnection -> do
          authSucceeded <-
            liftIO $
              SMTP.authenticate
                SMTP.LOGIN
                (T.unpack sendEmailSettingSMTPUsername)
                (T.unpack (unsafeShowPassword sendEmailSettingSMTPPassword))
                smtpConnection
          if authSucceeded
            then logInfoN "Authenticated"
            else liftIO $ die "Failed to authenticate on the SMTP server."

          liftIO $ SMTP.sendMail mail smtpConnection

          smosJobHuntInit
            settings
            InitSettings
              { initSettingCompany = company,
                initSettingContactName = sendEmailSettingToName,
                initSettingContactEmail = Just sendEmailSettingToAddress,
                initSettingUrl = sendEmailSettingUrl
              }
