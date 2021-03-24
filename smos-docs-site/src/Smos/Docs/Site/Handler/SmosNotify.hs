{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosNotify
  ( getSmosNotifyR,
  )
where

import qualified Env
import Options.Applicative
import Options.Applicative.Help
import Smos.Docs.Site.Handler.Import
import Smos.Notify.OptParse as Notify
import YamlParse.Applicative

getSmosNotifyR :: Handler Html
getSmosNotifyR = do
  DocPage {..} <- lookupPage "smos-notify"
  let argsHelpText = getHelpPageOf []
      envHelpText = Env.helpDoc Notify.prefixedEnvironmentParser
      confHelpText = prettySchemaDoc @Notify.Configuration
  defaultLayout $ do
    setSmosTitle "smos-notify"
    setDescription "Documentation for the Smos Notification tool"
    $(widgetFile "args")

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runArgumentsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "smos-notify"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
