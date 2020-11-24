{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.TUI
  ( getTUIR,
    getTUIInstanceR,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Path
import Smos.Web.Server.Foundation
import Smos.Web.Server.SmosSession
import Smos.Web.Server.TUI
import Smos.Web.Server.Widget
import Yesod hiding (Header)
import Yesod.WebSockets

getTUIR :: [Text] -> Handler Html
getTUIR ts = withLogin $ \_ -> do
  let terminalWidget = makeTerminalWidget $ TUIInstanceR ts
  withNavBar $ do
    setTitle "Smos Web TUI"
    $(widgetFile "tui")

getTUIInstanceR :: [Text] -> Handler ()
getTUIInstanceR ts = do
  withLogin' $ \userName token -> do
    case parseRelFile $ T.unpack $ T.intercalate "/" ts of
      Nothing -> notFound
      Just relFile -> do
        webSockets
          $ withSmosSession userName token relFile
          $ \instanceHandle -> communicateWithTerminal instanceHandle
