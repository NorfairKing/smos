{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosArchive
  ( getSmosArchiveR,
    getSmosArchiveCommandR,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Env
import Options.Applicative
import Options.Applicative.Help
import Smos.Archive.OptParse as Archive
import Smos.Docs.Site.Handler.Import

getSmosArchiveR :: Handler Html
getSmosArchiveR = do
  DocPage {..} <- lookupPage "smos-archive"
  let argsHelpText = getHelpPageOf []
      envHelpText = Env.helpDoc Archive.prefixedEnvironmentParser
      confHelpText = yamlDesc @Archive.Configuration
  defaultLayout $ do
    setSmosTitle "smos-archive"
    setDescriptionIdemp "Documentation for the Smos Archiving tool"
    $(widgetFile "args")

getSmosArchiveCommandR :: Text -> Handler Html
getSmosArchiveCommandR cmd = do
  DocPage {..} <- lookupPage' ["smos-archive", cmd]
  let argsHelpText = getHelpPageOf [T.unpack cmd]
      envHelpText = "This command does not use any extra environment variables." :: String
      confHelpText = case cmd of
        _ -> "This command admits no extra configuration." :: Text
  defaultLayout $ do
    setSmosTitle $ toHtml docPageTitle
    setDescriptionIdemp $ "Documentation for the " <> cmd <> " subcommand of the smos-archive tool"
    $(widgetFile "args")

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runArgumentsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "smos-archive"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
