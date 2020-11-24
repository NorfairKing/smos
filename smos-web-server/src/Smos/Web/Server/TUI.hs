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
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
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
          .| sanitiseUtf8Conduit
          .| sinkWSText
  runConduit (yield ("\ESC[?25h" :: Text) .| sinkWSText) -- turn on cursor
  res <- race (wait (terminalHandleAsync th)) (race (runConduit inputConduit) (runConduit outputConduit))
  case res of
    Left () -> do
      -- The smos instance quit succesfully.
      -- Send a succesful finish
      sendClose ("Success" :: Text)
    Right res' -> case res' of
      Left () -> do
        -- The browser disconnected
        -- There's nothing else we can do, just exit
        pure ()
      Right () -> do
        -- The smos instance's handle got closed
        -- No idea why this would happen.
        sendClose ("???" :: Text)

resizeConduit :: MonadIO m => TerminalHandle -> ConduitT ByteString ByteString m ()
resizeConduit instanceHandle = awaitForever $ \bs -> do
  case JSON.decode (LB.fromStrict bs) of
    Nothing -> yield bs -- It's not a resize request, just let it through.
    Just terminalSize -> liftIO $ terminalResize instanceHandle terminalSize

debugConduit :: MonadHandler m => String -> ConduitT ByteString ByteString m ()
debugConduit name = iterMC $ \bs -> do
  $(logDebug) (T.pack (name <> ": " <> show bs))
  pure ()

-- Just to see what happens if smos sends some non-utf8 for some reason
-- troubleConduit :: MonadHandler m => ConduitT ByteString ByteString m ()
-- troubleConduit = awaitForever $ \bs -> do
--   b <- liftIO $ (> (9 :: Int)) <$> randomRIO (1, 10)
--   when b $ yield $ SB.pack [0xc0]
--   yield bs

-- This makes sure that no non-utf8 gets to the client.
sanitiseUtf8Conduit :: Monad m => ConduitT ByteString Text m ()
sanitiseUtf8Conduit = C.map (TE.decodeUtf8With TE.ignore)
