{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosScheduler
  ( getSmosSchedulerR,
  )
where

import qualified Env
import Options.Applicative
import Options.Applicative.Help
import Smos.Docs.Site.Handler.Import
import Smos.Scheduler.OptParse as Scheduler
import YamlParse.Applicative

getSmosSchedulerR :: Handler Html
getSmosSchedulerR = do
  DocPage {..} <- lookupPage "smos-scheduler"
  let argsHelpText = getHelpPageOf []
      envHelpText = Env.helpDoc Scheduler.prefixedEnvironmentParser
      confHelpText = prettySchemaDoc @Scheduler.Configuration
  defaultLayout $ do
    setTitle "Smos Documentation - smos-scheduler"
    $(widgetFile "args")

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runArgumentsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "smos-scheduler"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
