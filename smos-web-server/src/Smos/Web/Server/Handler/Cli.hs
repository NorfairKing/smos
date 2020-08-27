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
import Smos.Web.Server.SmosSession
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
  withLogin' $ \userName token -> do
    instancesVar <- getsYesod appSmosInstances
    webSockets
      $ withSmosSession userName token instancesVar
      $ \instanceHandle ->
        withSavedInstance userName instancesVar instanceHandle $ do
          liftIO $ putStrLn "Starting to communicate"
          communicate instanceHandle

withSavedInstance :: MonadUnliftIO m => Username -> TVar (Map Username SmosInstanceHandle) -> SmosInstanceHandle -> m a -> m a
withSavedInstance un instancesVar i = bracket_ (addInstance i) (removeInstance i)
  where
    addInstance :: MonadUnliftIO m => SmosInstanceHandle -> m ()
    addInstance i = atomically $ modifyTVar' instancesVar $ M.insert un i
    removeInstance :: MonadUnliftIO m => SmosInstanceHandle -> m ()
    removeInstance i = atomically $ modifyTVar' instancesVar $ M.delete un

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
      Aeson.Success terminalSize -> do
        instancesVar <- getsYesod appSmosInstances
        mInstanceHandle <- liftIO $ M.lookup un <$> readTVarIO instancesVar
        case mInstanceHandle of
          Nothing -> sendStatusJSON badRequest400 $ makeError "There is no smos instance yet."
          Just instanceHandle -> do
            liftIO $ smosInstanceResize instanceHandle terminalSize
            pure $ object ["success" .= True]

communicate :: SmosInstanceHandle -> WebSocketsT Handler ()
communicate sih = do
  let inputConduit = sourceWS .| debugConduit "Input" .| smosInstanceInputSink sih
      outputConduit = smosInstanceOutputSource sih .| debugConduit "Output" .| sinkWSText
  runConduit (yield ("\ESC[?25h" :: Text) .| sinkWSText) -- turn on cursor
  inputAsync <- async $ runConduit inputConduit
  outputAsync <- async $ runConduit outputConduit
  void $ waitAnyCancel [inputAsync, outputAsync, smosInstanceHandleAsync sih]
  sendClose ("Close" :: Text)

debugConduit :: MonadIO m => String -> ConduitT ByteString ByteString m ()
debugConduit name = iterMC go
  where
    go _ = pure ()
-- go bs = liftIO $ putStrLn $ name <> ": " <> show bs
