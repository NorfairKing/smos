{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.TUI
  ( getTUIR,
    getTUIInstanceR,
  )
where

import qualified Data.Text as T
import Smos.Web.Server.Handler.Import

getTUIR :: Handler Html
getTUIR = withLogin $ \_ -> do
  navbar <- makeNavBar
  let terminalWidget = makeTerminalWidget TUIInstanceR
  defaultLayout $ do
    setTitle "Smos Web TUI"
    $(widgetFile "tui")

getTUIInstanceR :: Handler ()
getTUIInstanceR = do
  withLogin' $ \userName token ->
    webSockets $
      withSmosSession userName token $ \instanceHandle ->
        communicateWithTerminal instanceHandle
