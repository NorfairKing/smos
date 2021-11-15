{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosGitHub
  ( getSmosGitHubR,
  )
where

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

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runArgumentsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "smos-github"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
