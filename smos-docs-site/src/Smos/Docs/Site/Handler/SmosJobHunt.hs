{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.SmosJobHunt
  ( getSmosJobHuntR,
    getSmosJobHuntCommandR,
  )
where

import Data.Text (Text)
import Smos.Docs.Site.Handler.Import
import Smos.JobHunt.OptParse as JobHunt

getSmosJobHuntR :: Handler Html
getSmosJobHuntR = makeSettingsPage @JobHunt.Instructions "smos-jobhunt"

getSmosJobHuntCommandR :: Text -> Handler Html
getSmosJobHuntCommandR = makeCommandSettingsPage @JobHunt.Instructions "smos-jobhunt"
