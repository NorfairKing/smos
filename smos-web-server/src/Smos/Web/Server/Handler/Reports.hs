{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Reports
  ( getReportsR,
    getReportsInstanceR,
  )
where

import Smos.Web.Server.Handler.Import

getReportsR :: Handler Html
getReportsR = withLogin $ \_ -> do
  let terminalWidget = makeTerminalWidget ReportsInstanceR
  withNavBar $ do
    setTitle "Smos Web Reports"
    $(widgetFile "reports")

getReportsInstanceR :: Handler ()
getReportsInstanceR =
  withLogin' $ \userName token ->
    webSockets $
      withSmosShellSession userName token $
        \terminalHandle -> communicateWithTerminal terminalHandle
