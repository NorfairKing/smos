{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Web.Server.Foundation where

import Control.Arrow (left)
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad
import Control.Monad.Logger
import Data.Aeson as JSON
import qualified Data.ByteString.Base16 as Base16
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID.Typed as Typed
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types as Http
import Path
import Path.IO
import Servant.Auth.Client (Token (..))
import Smos.Client
import Smos.Web.Server.Constants
import Smos.Web.Server.Static
import Smos.Web.Server.Widget
import Smos.Web.Style
import Text.Hamlet
import Yesod
import Yesod.Auth
import qualified Yesod.Auth.Message as Msg
import Yesod.AutoReload
import Yesod.EmbeddedStatic

data App = App
  { appLogLevel :: !LogLevel,
    appAPIBaseUrl :: !BaseUrl,
    appDocsBaseUrl :: !(Maybe BaseUrl),
    appStatic :: !EmbeddedStatic,
    appStyle :: !EmbeddedStatic,
    appLoginTokens :: !(TVar (Map Username (Token, Bool))),
    appHttpManager :: !Http.Manager,
    appDataDir :: !(Path Abs Dir),
    appGoogleAnalyticsTracking :: !(Maybe Text),
    appGoogleSearchConsoleVerification :: !(Maybe Text)
  }

mkYesodData "App" $(parseRoutesFile "routes.txt")

instance Yesod App where
  shouldLogIO app _ ll = pure $ ll >= appLogLevel app
  defaultLayout widget = do
    app <- getYesod
    let addReloadWidget = if development then (<> autoReloadWidgetFor ReloadR) else id
    pageContent <- widgetToPageContent $ do
      addStylesheet $ StyleR index_css
      toWidgetHead [hamlet|<link rel="icon" href=@{StaticR favicon_ico} sizes="32x32" type="image/x-icon">|]
      addReloadWidget $(widgetFile "default-body")
    withUrlRenderer $ do
      $(hamletFile "templates/default-page.hamlet")
  authRoute _ = Just $ AuthR LoginR
  makeSessionBackend y = do
    clientSessionKeyFile <- resolveFile (appDataDir y) "client_session_key.aes"
    Just <$> defaultClientSessionBackend (60 * 24 * 365 * 10) (fromAbsFile clientSessionKeyFile)

instance YesodAuth App where
  type AuthId App = Username
  loginDest _ = HomeR
  logoutDest _ = HomeR
  authHttpManager = getsYesod appHttpManager
  authenticate creds =
    if credsPlugin creds == smosAuthPluginName
      then case parseUsername $ credsIdent creds of
        Nothing -> pure $ UserError Msg.InvalidLogin
        Just un -> pure $ Authenticated un
      else pure $ ServerError $ T.unwords ["Unknown authentication plugin:", credsPlugin creds]
  authPlugins _ = [smosAuthPlugin]
  maybeAuthId = do
    msv <- lookupSession credsKey
    case msv of
      Nothing -> pure Nothing
      Just sv -> pure $ fromPathPiece sv

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

smosAuthPluginName :: Text
smosAuthPluginName = "smos-auth-plugin"

{-# ANN smosAuthPlugin ("NOCOVER" :: String) #-}
smosAuthPlugin :: AuthPlugin App
smosAuthPlugin = AuthPlugin smosAuthPluginName dispatch loginWidget
  where
    dispatch :: Text -> [Text] -> AuthHandler App TypedContent
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch "GET" ["register"] = getNewAccountR >>= sendResponse
    dispatch "POST" ["register"] = postNewAccountR >>= sendResponse
    dispatch _ _ = notFound
    loginWidget :: (Route Auth -> Route App) -> Widget
    loginWidget _ = do
      token <- genToken
      msgs <- getMessages
      $(widgetFile "auth/login")

loginFormPostTargetR :: AuthRoute
loginFormPostTargetR = PluginR smosAuthPluginName ["login"]

usernameField ::
  Monad m =>
  RenderMessage (HandlerSite m) FormMessage =>
  Field m Username
usernameField = checkMMap (pure . left T.pack . parseUsernameWithError) usernameText textField

{-# ANN postLoginR ("NOCOVER" :: String) #-}
postLoginR :: AuthHandler App TypedContent
postLoginR = do
  let loginInputForm = Login <$> ireq usernameField "user" <*> ireq passwordField "passphrase"
  result <- runInputPostResult loginInputForm
  muser <-
    case result of
      FormMissing -> invalidArgs ["Form is missing"]
      FormFailure _ -> return $ Left Msg.InvalidLogin
      FormSuccess (Login ukey pwd) -> do
        liftHandler $ loginWeb $ Login {loginUsername = ukey, loginPassword = pwd}
        pure $ Right ukey
  case muser of
    Left err -> loginErrorMessageI LoginR err
    Right un -> do
      setCredsRedirect $ Creds smosAuthPluginName (usernameText un) []

registerR :: AuthRoute
registerR = PluginR smosAuthPluginName ["register"]

getNewAccountR :: AuthHandler App Html
getNewAccountR = do
  token <- genToken
  msgs <- getMessages
  liftHandler $ defaultLayout $(widgetFile "auth/register")

data NewAccount = NewAccount
  { newAccountUsername :: Username,
    newAccountPassword1 :: Text,
    newAccountPassword2 :: Text
  }
  deriving (Show)

postNewAccountR :: AuthHandler App TypedContent
postNewAccountR = do
  let newAccountInputForm =
        NewAccount
          <$> ireq
            ( checkMMap
                ( \t ->
                    pure $
                      case parseUsernameWithError t of
                        Left err -> Left (T.pack $ unwords ["Invalid username:", show t ++ ";", err])
                        Right un -> Right un
                )
                usernameText
                textField
            )
            "username"
          <*> ireq passwordField "passphrase"
          <*> ireq passwordField "passphrase-confirm"
  mr <- liftHandler getMessageRender
  result <- liftHandler $ runInputPostResult newAccountInputForm
  mdata <-
    case result of
      FormMissing -> invalidArgs ["Form is incomplete"]
      FormFailure msgs -> pure $ Left msgs
      FormSuccess d ->
        pure $
          if newAccountPassword1 d == newAccountPassword2 d
            then
              Right
                Register
                  { registerUsername = newAccountUsername d,
                    registerPassword = newAccountPassword1 d
                  }
            else Left [mr Msg.PassMismatch]
  case mdata of
    Left errs -> do
      setMessage $ toHtml $ T.concat errs
      liftHandler $ redirect $ AuthR registerR
    Right reg -> do
      errOrOk <- liftHandler $ runClientSafe $ clientPostRegister reg
      case errOrOk of
        Left err -> do
          case err of
            FailureResponse _ resp ->
              case Http.statusCode $ responseStatusCode resp of
                409 -> setMessage "An account with this username already exists"
                i -> setMessage $ "Failed to register for unknown reasons, got exit code: " <> toHtml (show i)
            _ -> setMessage "Failed to register for unknown reasons."
          liftHandler $ redirect $ AuthR registerR
        Right NoContent ->
          liftHandler $ do
            loginWeb
              Login {loginUsername = registerUsername reg, loginPassword = registerPassword reg}
            setCredsRedirect $ Creds smosAuthPluginName (usernameText $ registerUsername reg) []

loginWeb :: Login -> Handler ()
loginWeb form = do
  errOrRes <- runClientSafe $ clientLogin form
  case errOrRes of
    Left err ->
      handleStandardServantErrs err $ \resp ->
        if responseStatusCode resp == Http.unauthorized401
          then do
            addMessage "is-danger" "Unable to login"
            redirect $ AuthR LoginR
          else error $ show resp
    Right (Left _) -> undefined
    Right (Right t) -> do
      UserPermissions {..} <- runClientOrErr $ clientGetUserPermissions t
      recordLoginToken (loginUsername form) t userPermissionsIsAdmin

handleStandardServantErrs :: ClientError -> (Response -> Handler a) -> Handler a
handleStandardServantErrs err func =
  case err of
    FailureResponse _ resp ->
      if responseStatusCode resp == Http.badGateway502
        then error $ unwords ["The api seems to be down:", show resp]
        else func resp
    ConnectionError e -> error $ unwords ["The api seems to be down:", show e]
    e -> error $ unwords ["Error while calling API:", show e]

-- Nothing means not logged in
userIsAdmin :: Handler (Maybe Bool)
userIsAdmin = do
  mun <- maybeAuthId
  case mun of
    Nothing -> pure Nothing
    Just un -> fmap snd <$> lookupToginToken un

withLogin :: (Token -> Handler a) -> Handler a
withLogin func = withLogin' $ \_ t -> func t

withAdminLogin :: (Token -> Handler a) -> Handler a
withAdminLogin func = withAdminLogin' (\_ t -> func t)

withAdminLogin' :: (Username -> Token -> Handler a) -> Handler a
withAdminLogin' func = withLogin'' $ \admin un t ->
  if admin
    then func un t
    else notFound

withLogin' :: (Username -> Token -> Handler a) -> Handler a
withLogin' func = withLogin'' (\_ un t -> func un t)

withLogin'' :: (Bool -> Username -> Token -> Handler a) -> Handler a
withLogin'' func = do
  un <- requireAuthId
  mLoginToken <- lookupToginToken un
  case mLoginToken of
    Nothing -> redirect $ AuthR LoginR
    Just (token, admin) -> func admin un token

lookupToginToken :: Username -> Handler (Maybe (Token, Bool))
lookupToginToken un = do
  readTokens
  tokenMapVar <- getsYesod appLoginTokens
  tokenMap <- liftIO $ readTVarIO tokenMapVar
  pure $ M.lookup un tokenMap

recordLoginToken :: Username -> Token -> Bool -> Handler ()
recordLoginToken un token isAdmin = do
  tokenMapVar <- getsYesod appLoginTokens
  liftIO $ atomically $ modifyTVar tokenMapVar $ M.insert un (token, isAdmin)
  writeTokens

deleteLoginToken :: Username -> Handler ()
deleteLoginToken un = do
  tokenMapVar <- getsYesod appLoginTokens
  liftIO $ atomically $ modifyTVar tokenMapVar $ M.delete un
  writeTokens

-- These three are used so you don't have to log in again every time you restart the server during development
developmentTokenFile :: FilePath
developmentTokenFile = "smos-web-server-tokens.json"

readTokens :: Handler ()
readTokens = when development $ do
  tokenMapVar <- getsYesod appLoginTokens
  liftIO $ do
    mts <- fmap join $ forgivingAbsence $ JSON.decodeFileStrict' developmentTokenFile
    atomically $ writeTVar tokenMapVar $ fromMaybe M.empty mts

writeTokens :: Handler ()
writeTokens = when development $ do
  tokenMapVar <- getsYesod appLoginTokens
  liftIO $ do
    tokenMap <- readTVarIO tokenMapVar
    JSON.encodeFile developmentTokenFile tokenMap

instance FromJSON Token where
  parseJSON =
    withText "Token" $ \t ->
      case Base16.decode $ TE.encodeUtf8 t of
        Right h -> pure $ Token h
        Left err -> fail $ "Invalid token in JSON: " <> err

instance ToJSON Token where
  toJSON (Token bs) =
    case TE.decodeUtf8' $ Base16.encode bs of
      Left _ -> error "Failed to decode hex string to text, should not happen."
      Right t -> JSON.String t

genToken :: MonadHandler m => m Html
genToken = do
  t <- getCSRFToken
  let tokenKey = defaultCsrfParamName
  pure [shamlet|<input type=hidden name=#{tokenKey} value=#{t}>|]

getCSRFToken :: MonadHandler m => m Text
getCSRFToken = fromMaybe "" . reqToken <$> getRequest

runClientSafe :: NFData a => ClientM a -> Handler (Either ClientError a)
runClientSafe func = do
  man <- getsYesod appHttpManager
  burl <- getsYesod appAPIBaseUrl
  let cenv = mkClientEnv man burl
  liftIO $ runClient cenv func

runClientOrErr :: NFData a => ClientM a -> Handler a
runClientOrErr func = do
  errOrRes <- runClientSafe func
  case errOrRes of
    Left err -> handleStandardServantErrs err $ \resp -> error $ show resp -- TODO deal with error
    Right r -> pure r

runClientOrDisallow :: NFData a => ClientM a -> Handler (Maybe a)
runClientOrDisallow func = do
  errOrRes <- runClientSafe func
  case errOrRes of
    Left err ->
      handleStandardServantErrs err $ \resp ->
        if responseStatusCode resp == Http.unauthorized401
          then pure Nothing
          else error $ show resp -- TODO deal with error
    Right r -> pure $ Just r

runClientOrNotFound :: NFData a => ClientM a -> Handler (Maybe a)
runClientOrNotFound func = do
  errOrRes <- runClientSafe func
  case errOrRes of
    Left err ->
      handleStandardServantErrs err $ \resp ->
        if responseStatusCode resp == Http.notFound404
          then pure Nothing
          else error $ show resp -- TODO deal with error
    Right r -> pure $ Just r

usernameToPath :: Username -> FilePath
usernameToPath = T.unpack . toHexText . hashBytes . TE.encodeUtf8 . usernameText

userDataDir :: (MonadHandler m, HandlerSite m ~ App) => Username -> m (Path Abs Dir)
userDataDir un = do
  dataDir <- getsYesod appDataDir
  usersDataDir <- resolveDir dataDir "users"
  resolveDir usersDataDir $ usernameToPath un

withNavBar :: Widget -> Handler Html
withNavBar body = do
  navbar <- makeNavBar
  let footer = $(widgetFile "footer")
  defaultLayout
    [whamlet|
      ^{navbar}
      ^{body}
      ^{footer}
    |]

makeNavBar :: Handler Widget
makeNavBar = do
  mAdmin <- userIsAdmin
  mDocsUrl <- getsYesod appDocsBaseUrl
  msgs <- getMessages
  pure $(widgetFile "navbar")

instance PathPiece (Typed.UUID a) where
  fromPathPiece = Typed.parseUUIDText
  toPathPiece = Typed.uuidText
