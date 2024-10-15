{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosScheduler
  ( getSmosSchedulerR,
    getSmosSchedulerCommandR,
    getSmosSchedulerTemplateR,
    getSmosSchedulerNixosR,
  )
where

import Data.Text (Text)
import Smos.Docs.Site.Handler.Import
import Smos.Docs.Site.Handler.Page
import Smos.Scheduler.OptParse as Scheduler
import Text.RawString.QQ

getSmosSchedulerR :: Handler Html
getSmosSchedulerR = makeSettingsPage @Scheduler.Instructions "smos-scheduler"

getSmosSchedulerCommandR :: Text -> Handler Html
getSmosSchedulerCommandR = makeCommandSettingsPage @Scheduler.Instructions "smos-scheduler"

getSmosSchedulerNixosR :: Handler Html
getSmosSchedulerNixosR = getPageR ["smos-scheduler", "nixos"]

getSmosSchedulerTemplateR :: Handler Html
getSmosSchedulerTemplateR = do
  let confHelpText = yamlDesc @Scheduler.ScheduleTemplate
  defaultLayout $ do
    setSmosTitle "smos-scheduler templates"
    setDescriptionIdemp "Documentation for smos-scheduler template file format"
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
