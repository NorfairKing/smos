{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosScheduler
  ( getSmosSchedulerR,
    getSmosSchedulerTemplateR,
  )
where

import qualified Env
import Options.Applicative
import Options.Applicative.Help
import Smos.Docs.Site.Handler.Import
import Smos.Scheduler.OptParse as Scheduler
import Text.RawString.QQ

getSmosSchedulerR :: Handler Html
getSmosSchedulerR = do
  DocPage {..} <- lookupPage "smos-scheduler"
  let argsHelpText = getHelpPageOf []
      envHelpText = Env.helpDoc Scheduler.prefixedEnvironmentParser
      confHelpText = yamlDesc @Scheduler.Configuration
  defaultLayout $ do
    setSmosTitle "smos-scheduler"
    setDescription "Documentation for the Smos Scheduler tool"
    $(widgetFile "args")

getSmosSchedulerTemplateR :: Handler Html
getSmosSchedulerTemplateR = do
  let confHelpText = yamlDesc @Scheduler.ScheduleTemplate
  defaultLayout $ do
    setSmosTitle "smos-scheduler templates"
    setDescription "Documentation for smos-scheduler template file format"
    $(widgetFile "smos-scheduler/template")

templateExample :: String
templateExample =
  [r|
- entry: Weekly actions
  forest:
  - Clean room
  - header: Weekly review
    state: READY
    properties:
      timewindow: 1h
    tags:
    - review
    timestamps:
      SCHEDULED: "[ %F | saturday ]"|]

getHelpPageOf :: [String] -> String
getHelpPageOf args =
  let res = runArgumentsParser $ args ++ ["--help"]
   in case res of
        Failure fr ->
          let (ph, _, cols) = execFailure fr "smos-scheduler"
           in renderHelp cols ph
        _ -> error "Something went wrong while calling the option parser."
