{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Playground
  ( getPlaygroundR,
    getPlaygroundInstanceR,
  )
where

import Smos.Web.Server.Handler.Import

getPlaygroundR :: Handler Html
getPlaygroundR = do
  navbar <- makeNavBar
  let terminalWidget = makeTerminalWidget PlaygroundInstanceR
  mDocsUrl <- getsYesod appDocsBaseUrl
  defaultLayout $ do
    setTitle "Smos Web TUI Playground"
    $(widgetFile "playground")

getPlaygroundInstanceR :: Handler ()
getPlaygroundInstanceR = do
  webSockets $
    withPlaygroundSession playgroundFile $ \instanceHandle ->
      communicateWithTerminal instanceHandle

playgroundFile :: Path Rel File
playgroundFile = [relfile|playground.smos|]
