{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosGitHub
  ( getSmosGitHubR,
    getSmosGitHubCommandR,
  )
where

import Data.Text (Text)
import Smos.Docs.Site.Handler.Import
import Smos.GitHub.OptParse as GitHub

getSmosGitHubR :: Handler Html
getSmosGitHubR = makeSettingsPage @GitHub.Instructions "smos-github"

getSmosGitHubCommandR :: Text -> Handler Html
getSmosGitHubCommandR = makeCommandSettingsPage @GitHub.Instructions "smos-github"
