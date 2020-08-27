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
import UnliftIO hiding (Handler)
import Yesod hiding (Header)
import Yesod.WebSockets

getCliR :: Handler Html
getCliR = withLogin' $ \un _ -> do
  instancesVar <- getsYesod appSmosInstances
  M.keys <$> readTVarIO instancesVar >>= liftIO . print
  defaultLayout $ do
    setTitle "Smos Web TUI"
    addScript $ StaticR xterm_js
    addScript $ StaticR xterm_attach_js
    addScript $ StaticR xterm_fit_js
    addStylesheet $ StaticR xterm_css
    addScript $ StaticR jquery_js
    $(widgetFile "cli")

getInstanceR :: Handler ()
getInstanceR = do
  withLogin' $ \un t -> do
    instancesVar <- getsYesod appSmosInstances
    webSockets
      $ withSession un instancesVar
      $ \instanceHandle -> do
        liftIO $ putStrLn "Starting to communicate"
        communicate instanceHandle

withSession :: MonadUnliftIO m => Username -> TVar (Map Username SmosInstanceHandle) -> (SmosInstanceHandle -> m a) -> m a
withSession un instancesVar func = do
  mInstance <- M.lookup un <$> readTVarIO instancesVar
  case mInstance of
    Nothing -> bracket readyDir unreadyDir $ \workflowDir -> do
      startingFile <- liftIO $ resolveFile workflowDir "example.smos"
      withSmosInstance workflowDir startingFile $ \i ->
        bracket_ (addInstance i) (removeInstance i) (func i)
    Just i -> do
      liftIO $ putStrLn "Should not happen."
      -- Should not happen, but it's fine if it does, I guess...
      -- We'll assume that whatever opened this instance will deal with cleanup as well.
      func i
  where
    addInstance :: MonadUnliftIO m => SmosInstanceHandle -> m ()
    addInstance i = atomically $ modifyTVar' instancesVar $ M.insert un i
    removeInstance :: MonadUnliftIO m => SmosInstanceHandle -> m ()
    removeInstance i = atomically $ modifyTVar' instancesVar $ M.delete un
    readyDir :: MonadUnliftIO m => m (Path Abs Dir)
    readyDir = do
      liftIO $ putStrLn "Loading dir"
      liftIO $ putStrLn "Creating a new instance"
      workflowDir <- resolveDir' "/tmp/smos-web-server/example-workflow-dir"
      -- TODO Load the dir
      ensureDir workflowDir
      pure workflowDir
    unreadyDir :: MonadUnliftIO m => Path Abs Dir -> m ()
    unreadyDir workflowDir = do
      liftIO $ putStrLn "Unloading dir"
      removeDirRecur workflowDir -- TODO save the dir

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
        mInstanceHandle <- liftIO $ M.lookup un <$> readTVarIO instancesVar
        case mInstanceHandle of
          Nothing -> sendStatusJSON badRequest400 $ makeError "There is no smos instance yet."
          Just instanceHandle -> do
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

communicate :: SmosInstanceHandle -> WebSocketsT Handler ()
communicate sih = do
  let inputConduit = sourceWS .| debugConduit "Input" .| smosInstanceInputSink sih
      outputConduit = smosInstanceOutputSource sih .| debugConduit "Output" .| sinkWSText
  runConduit (yield ("\ESC[?25h" :: Text) .| sinkWSText) -- turn on cursor
  inputAsync <- async $ runConduit inputConduit
  outputAsync <- async $ runConduit outputConduit
  ioAsync <- async $ waitEither inputAsync outputAsync
  ress <- waitEither (smosInstanceHandleAsync sih) ioAsync
  case ress of
    Left () -> do
      -- Smos exited
      sendClose ("Close" :: Text)
      liftIO $ destroySmosInstance sih
    Right res ->
      -- Something went wrong with the communications?
      case res of
        Left () -> cancel outputAsync
        Right () -> cancel inputAsync

debugConduit :: MonadIO m => String -> ConduitT ByteString ByteString m ()
debugConduit name = iterMC go
  where
    -- go _ = pure ()
    go bs = liftIO $ putStrLn $ name <> ": " <> show bs
