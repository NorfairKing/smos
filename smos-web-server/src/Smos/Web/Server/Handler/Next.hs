{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Next where

import Path
import Smos.Client
import Smos.Data
import Smos.Report.Next
import Smos.Web.Server.Foundation
import Smos.Web.Server.Widget
import Yesod

getReportNextR :: Handler Html
getReportNextR = withLogin $ \t -> do
  report <- runClientOrErr $ clientGetNextActionReport t
  defaultLayout $(widgetFile "next")
