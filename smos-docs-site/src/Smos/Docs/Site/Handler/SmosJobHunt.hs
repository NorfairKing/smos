{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosJobHunt
  ( getSmosJobHuntR,
    getSmosJobHuntCommandR,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Env
import Options.Applicative
import Options.Applicative.Help
import Smos.Docs.Site.Handler.Import
import Smos.JobHunt.OptParse as JobHunt

getSmosJobHuntR :: Handler Html
getSmosJobHuntR = do
  DocPage {..} <- lookupPage "smos-jobhunt"
  let argsHelpText = getHelpPageOf []
      envHelpText = Env.helpDoc JobHunt.prefixedEnvironmentParser
      confHelpText = yamlDesc @JobHunt.Configuration
  defaultLayout $ do
    setSmosTitle "smos-jobhunt"
    setDescriptionIdemp "Documentation for the Smos JobHunt tool"
    $(widgetFile "args")

getSmosJobHuntCommandR :: Text -> Handler Html
getSmosJobHuntCommandR cmd = do
  DocPage {..} <- lookupPage' ["smos-jobhunt", cmd]
  let argsHelpText = getHelpPageOf [T.unpack cmd]
      envHelpText = case cmd of
        "init" -> "The init command does not use any extra environment variables."
        "email" -> Env.helpDoc prefixedSendEmailEnvironmentParser
        _ -> "This command does not use any extra environment variables." :: String
      confHelpText :: Text
      confHelpText = case cmd of
        "init" -> "The init command admits no extra configuration."
        "email" -> yamlDesc @Configuration
        _ -> "This command admits no extra configuration."
  defaultLayout $ do
    setSmosTitle $ toHtml docPageTitle
    setDescriptionIdemp $ "Documentation for the " <> cmd <> " subcommand of the smos-jobhunt tool"
    $(widgetFile "args")

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = JobHunt.runArgumentsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "smos-jobhunt"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
