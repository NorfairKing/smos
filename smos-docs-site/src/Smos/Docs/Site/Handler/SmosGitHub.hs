{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosGitHub
  ( getSmosGitHubR,
    getSmosGitHubCommandR,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Env
import Options.Applicative
import Options.Applicative.Help
import Smos.Docs.Site.Handler.Import
import Smos.GitHub.OptParse as GitHub

getSmosGitHubR :: Handler Html
getSmosGitHubR = do
  DocPage {..} <- lookupPage "smos-github"
  let argsHelpText = getHelpPageOf []
      envHelpText = Env.helpDoc GitHub.prefixedEnvironmentParser
      confHelpText = yamlDesc @GitHub.Configuration
  defaultLayout $ do
    setSmosTitle "smos-github"
    setDescription "Documentation for the Smos GitHub tool"
    $(widgetFile "args")

getSmosGitHubCommandR :: Text -> Handler Html
getSmosGitHubCommandR cmd = do
  DocPage {..} <- lookupPage' ["smos-github", cmd]
  let argsHelpText = getHelpPageOf [T.unpack cmd]
      envHelpText = "This command does not use any extra environment variables." :: String
      confHelpText :: Text
      confHelpText = case cmd of
        "list" -> "The list command admits no extra configuration."
        "import" -> "The import command admits no extra configuration."
        _ -> "This command admits no extra configuration."
  defaultLayout $ do
    setSmosTitle $ toHtml docPageTitle
    setDescription $ "Documentation for the " <> cmd <> " subcommand of the smos-github tool"
    $(widgetFile "args")

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runArgumentsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "smos-github"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
