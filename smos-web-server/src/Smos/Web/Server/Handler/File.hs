{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.File
  ( getFileR,
    getInstanceR,
  )
where

import Conduit
import Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import Path
import Servant.Client
import Smos.Instance
import Smos.Web.Server.Foundation
import Smos.Web.Server.SmosSession
import Smos.Web.Server.Static
import Smos.Web.Server.Widget
import UnliftIO hiding (Handler)
import Yesod hiding (Header)
import Yesod.Auth
import Yesod.WebSockets

getFileR :: [Text] -> Handler Html
getFileR ts = withLogin $ \_ ->
  withNavBar $ do
    setTitle "Smos Web TUI"
    addScript $ StaticR xterm_js
    addScript $ StaticR xterm_attach_js
    addScript $ StaticR xterm_fit_js
    addStylesheet $ StaticR xterm_css
    addScript $ StaticR jquery_js
    $(widgetFile "file")

getInstanceR :: [Text] -> Handler ()
getInstanceR ts = do
  withLogin' $ \userName token -> do
    case parseRelFile $ T.unpack $ T.intercalate "/" ts of
      Nothing -> notFound
      Just relFile -> do
        webSockets
          $ withSmosSession userName token relFile
          $ \instanceHandle -> communicate instanceHandle

communicate :: SmosInstanceHandle -> WebSocketsT Handler ()
communicate sih = do
  let inputConduit = sourceWS .| debugConduit "Input" .| resizeConduit sih .| smosInstanceInputSink sih
      outputConduit = smosInstanceOutputSource sih .| debugConduit "Output" .| sinkWSText
  runConduit (yield ("\ESC[?25h" :: Text) .| sinkWSText) -- turn on cursor
  res <- race (wait (smosInstanceHandleAsync sih)) (race (runConduit inputConduit) (runConduit outputConduit))
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

resizeConduit :: MonadIO m => SmosInstanceHandle -> ConduitT ByteString ByteString m ()
resizeConduit instanceHandle = awaitForever $ \bs -> do
  case JSON.decode (LB.fromStrict bs) of
    Nothing -> yield bs -- It's not a resize request, just let it through.
    Just terminalSize -> liftIO $ smosInstanceResize instanceHandle terminalSize

debugConduit :: MonadIO m => String -> ConduitT ByteString ByteString m ()
debugConduit _ = iterMC $ \_ -> do
  -- liftIO $ threadDelay $ 150 * 1000 -- Artificial delay
  pure ()
-- debugConduit name = iterMC go
--   where
--     go bs = liftIO $ putStrLn $ name <> ": " <> show bs
