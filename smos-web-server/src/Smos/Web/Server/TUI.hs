{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.TUI
  ( makeTerminalWidget,
    communicateWithTerminal,
  )
where

import Conduit
import Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import Smos.Terminal
import Smos.Web.Server.Foundation
import Smos.Web.Server.Static
import Smos.Web.Server.Widget
import UnliftIO hiding (Handler)
import Yesod hiding (Header)
import Yesod.WebSockets

makeTerminalWidget :: Route App -> Widget
makeTerminalWidget websocketRoute = do
  addScript $ StaticR xterm_js
  addScript $ StaticR xterm_attach_js
  addScript $ StaticR xterm_fit_js
  addStylesheet $ StaticR xterm_css
  addScript $ StaticR jquery_js
  $(widgetFile "terminal")

communicateWithTerminal :: TerminalHandle -> WebSocketsT Handler ()
communicateWithTerminal th = do
  let inputConduit =
        sourceWS
          .| debugConduit "Input"
          .| resizeConduit th
          .| terminalInputSink th
      outputConduit =
        terminalOutputSource th
          .| debugConduit "Output"
          .| sinkWSBinary
  runConduit (yield ("\ESC[?25h" :: Text) .| sinkWSText) -- turn on cursor
  res <- race (wait (terminalHandleAsync th)) (race (runConduit inputConduit) (runConduit outputConduit))
  case res of
    Left () -> do
      -- The smos instance quit succesfully.
      -- Send a succesful finish
      $(logInfo) "The smos instance quit sucessfully"
      sendClose ("Success" :: Text)
    Right res' -> case res' of
      Left () -> do
        -- The browser disconnected
        -- There's nothing else we can do, just exit
        $(logWarn) "The browser disconnected."
        pure ()
      Right () -> do
        -- The smos instance's handle got closed
        -- No idea why this would happen.
        $(logWarn) "The smos instance's handle got closed."
        sendClose ("???" :: Text)

resizeConduit :: (MonadIO m) => TerminalHandle -> ConduitT ByteString ByteString m ()
resizeConduit instanceHandle = awaitForever $ \bs -> do
  case JSON.decode (LB.fromStrict bs) of
    Nothing -> yield bs -- It's not a resize request, just let it through.
    Just terminalSize -> liftIO $ terminalResize instanceHandle terminalSize

debugConduit :: (MonadHandler m) => String -> ConduitT ByteString ByteString m ()
debugConduit name = iterMC $ \bs -> do
  $(logDebug) (T.pack (name <> ": " <> show bs))
  pure ()
