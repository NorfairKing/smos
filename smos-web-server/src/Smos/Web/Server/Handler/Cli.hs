{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Cli
  ( getCliR,
    getInstanceR,
    postResizeR,
  )
where

import Conduit
import Control.Concurrent.STM
import Control.Monad
import Data.Aeson as Aeson
import Data.Aeson.Text as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types.Status (badRequest400)
import Path
import Path.IO
import Smos.Client hiding (Header)
import Smos.Data
import Smos.Web.Server.Foundation
import Smos.Web.Server.SmosInstance
import Smos.Web.Server.Static
import Smos.Web.Server.Widget
import Text.Julius
import Text.Show.Pretty (ppShow)
import Yesod hiding (Header)
import Yesod.WebSockets

getCliR :: Handler Html
getCliR = withLogin' $ \un t -> do
  defaultLayout $ do
    setTitle "Smos Web TUI"
    addScript $ StaticR hterm_js
    addScript $ StaticR jquery_js
    $(widgetFile "cli")

getInstanceR :: Handler ()
getInstanceR =
  withLogin' $ \un t -> do
    -- TODO figure out a way to deal with logins here.
    instancesVar <- getsYesod appSmosInstances
    instanceHandle <- liftIO $ getOrCreateSmosInstance un instancesVar
    webSockets $ communicate instanceHandle

postResizeR :: Handler Value
postResizeR = do
  jb <- parseJsonBody
  withLogin' $ \un t -> do
    let makeError :: Text -> Value
        makeError txt = object ["error" .= txt]
    case jb of
      Aeson.Error err ->
        sendStatusJSON badRequest400 $
          makeError ("JSON Parse Error: " <> T.pack err)
      Aeson.Success TerminalSize {..} -> do
        instancesVar <- getsYesod appSmosInstances
        instanceHandle <- liftIO $ getOrCreateSmosInstance un instancesVar
        liftIO $ smosInstanceResize instanceHandle tsWidth tsHeight
        -- TODO deal with failed resizing
        pure $ object ["success" .= True]

data TerminalSize
  = TerminalSize
      { tsWidth :: !Word,
        tsHeight :: !Word
      }
  deriving (Show)

instance FromJSON TerminalSize where
  parseJSON =
    withObject "TerminalSize" $ \o -> do
      tsWidth <- o .: "width"
      tsHeight <- o .: "height"
      pure TerminalSize {..}

getOrCreateSmosInstance :: Username -> TVar (Map Username SmosInstanceHandle) -> IO SmosInstanceHandle
getOrCreateSmosInstance un var = do
  mInstance <- M.lookup un <$> readTVarIO var
  size <- M.size <$> readTVarIO var
  case mInstance of
    Just i -> pure i
    Nothing -> do
      workflowDir <- resolveDir' "/tmp/smos-web-server/example-workflow-dir"
      ensureDir workflowDir
      startingFile <- resolveFile workflowDir "example.smos"
      i <- makeSmosInstance workflowDir startingFile
      atomically $ modifyTVar' var $ M.insert un i
      pure i

communicate :: SmosInstanceHandle -> WebSocketsT Handler ()
communicate sih@SmosInstanceHandle {..} = do
  let inputConduit = sourceWS .| debugConduit "Input" .| smosInstanceInputSink sih
      outputConduit = smosInstanceOutputSource sih .| debugConduit "Output" .| sinkWSText
      runCommunication = do
        runConduit (yield ("\ESC[?25h" :: Text) .| sinkWSText) -- turn on cursor
        race_ (runConduit outputConduit) (runConduit inputConduit)
  runCommunication

debugConduit :: MonadIO m => String -> ConduitT ByteString ByteString m ()
debugConduit name = iterMC go
  where
    go bs = liftIO $ putStrLn $ name <> ": " <> show bs
