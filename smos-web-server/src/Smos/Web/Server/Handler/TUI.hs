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

getTUIR :: [Text] -> Handler Html
getTUIR ts = withLogin $ \_ -> do
  navbar <- makeNavBar
  let terminalWidget = makeTerminalWidget $ TUIInstanceR ts
  defaultLayout $ do
    setTitle "Smos Web TUI"
    $(widgetFile "tui")

getTUIInstanceR :: [Text] -> Handler ()
getTUIInstanceR ts = do
  withLogin' $ \userName token -> do
    case parseRelFile $ T.unpack $ T.intercalate "/" ts of
      Nothing -> notFound
      Just relFile -> do
        webSockets $
          withSmosSession userName token relFile $
            \instanceHandle -> communicateWithTerminal instanceHandle
