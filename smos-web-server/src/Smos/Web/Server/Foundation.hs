{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Web.Server.Foundation where

import Control.Arrow (left)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Logger
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LB
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types as Http
import Path
import Path.IO
import Servant.Auth.Client (Token (..))
import Servant.Client
import Smos.Client
import Smos.Web.Server.Constants
import Smos.Web.Server.Widget
import Text.Hamlet
import Yesod
import Yesod.Auth
import qualified Yesod.Auth.Message as Msg
import Yesod.EmbeddedStatic

data App
  = App
      { appLogLevel :: !LogLevel,
        appAPIBaseUrl :: !BaseUrl,
        appStatic :: !EmbeddedStatic,
        appLoginTokens :: !(TVar (Map Username Token)),
        appHttpManager :: !Http.Manager
      }

mkYesodData "App" $(parseRoutesFile "routes.txt")

instance Yesod App where
  shouldLogIO app _ ll = pure $ ll >= appLogLevel app
  defaultLayout widget = do
    pageContent <- widgetToPageContent $ do
      $(widgetFile "default-body")
    withUrlRenderer $ do
      $(hamletFile "templates/default-page.hamlet")
  authRoute _ = Just $ AuthR LoginR

instance YesodAuth App where
  type AuthId App = Username
  loginDest _ = HomeR
  logoutDest _ = HomeR
  authHttpManager = getsYesod appHttpManager
  authenticate creds =
    if credsPlugin creds == donnaAuthPluginName
      then case parseUsername $ credsIdent creds of
        Nothing -> pure $ UserError Msg.InvalidLogin
        Just un -> pure $ Authenticated un
      else pure $ ServerError $ T.unwords ["Unknown authentication plugin:", credsPlugin creds]
  authPlugins _ = [donnaAuthPlugin]
  maybeAuthId = do
    msv <- lookupSession credsKey
    case msv of
      Nothing -> pure Nothing
      Just sv -> pure $ fromPathPiece sv

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

donnaAuthPluginName :: Text
donnaAuthPluginName = "donna-auth-plugin"

donnaAuthPlugin :: AuthPlugin App
donnaAuthPlugin = AuthPlugin donnaAuthPluginName dispatch loginWidget
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
loginFormPostTargetR = PluginR donnaAuthPluginName ["login"]

usernameField ::
  Monad m =>
  RenderMessage (HandlerSite m) FormMessage =>
  Field m Username
usernameField = checkMMap (pure . left T.pack . parseUsernameWithError) usernameText textField

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
      setCredsRedirect $ Creds donnaAuthPluginName (usernameText un) []

registerR :: AuthRoute
registerR = PluginR donnaAuthPluginName ["register"]

getNewAccountR :: AuthHandler App Html
getNewAccountR = do
  token <- genToken
  msgs <- getMessages
  liftHandler $ defaultLayout $(widgetFile "auth/register")

data NewAccount
  = NewAccount
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
                _ -> setMessage "Failed to register for unknown reasons."
            _ -> setMessage "Failed to register for unknown reasons."
          liftHandler $ redirect $ AuthR registerR
        Right NoContent ->
          liftHandler $ do
            loginWeb
              Login {loginUsername = registerUsername reg, loginPassword = registerPassword reg}
            setCredsRedirect $ Creds donnaAuthPluginName (usernameText $ registerUsername reg) []

loginWeb :: Login -> Handler ()
loginWeb form = do
  errOrRes <- runClientSafe $ clientLogin form
  case errOrRes of
    Left err ->
      handleStandardServantErrs err $ \resp ->
        if responseStatusCode resp == Http.unauthorized401
          then do
            addMessage "error" "Unable to login"
            redirect $ AuthR LoginR
          else error $ show resp
    Right (Left _) -> undefined
    Right (Right t) -> recordLoginToken (loginUsername form) t

handleStandardServantErrs :: ClientError -> (Response -> Handler a) -> Handler a
handleStandardServantErrs err func =
  case err of
    FailureResponse _ resp -> func resp
    ConnectionError e -> error $ unwords ["The api seems to be down:", show e]
    e -> error $ unwords ["Error while calling API:", show e]

withLogin :: (Token -> Handler Html) -> Handler Html
withLogin func = do
  un <- requireAuthId
  mLoginToken <- lookupToginToken un
  case mLoginToken of
    Nothing -> redirect $ AuthR LoginR
    Just token -> func token

lookupToginToken :: Username -> Handler (Maybe Token)
lookupToginToken un = do
  whenPersistLogins loadLogins
  tokenMapVar <- getsYesod appLoginTokens
  tokenMap <- liftIO $ readTVarIO tokenMapVar
  pure $ M.lookup un tokenMap

recordLoginToken :: Username -> Token -> Handler ()
recordLoginToken un token = do
  tokenMapVar <- getsYesod appLoginTokens
  liftIO $ atomically $ modifyTVar tokenMapVar $ M.insert un token
  whenPersistLogins storeLogins

whenPersistLogins :: Handler () -> Handler ()
whenPersistLogins = when development

loadLogins :: Handler ()
loadLogins = do
  tokenMapVar <- getsYesod appLoginTokens
  liftIO $ do
    logins <- readLogins
    atomically $ modifyTVar tokenMapVar $ \m -> fromMaybe m logins

storeLogins :: Handler ()
storeLogins = do
  tokenMapVar <- getsYesod appLoginTokens
  liftIO $ do
    m <- readTVarIO tokenMapVar
    writeLogins m

loginsFile :: IO (Path Abs File)
loginsFile = resolveFile' "logins.json"

readLogins :: IO (Maybe (Map Username Token))
readLogins = do
  lf <- loginsFile
  mErrOrLogins <- forgivingAbsence $ JSON.eitherDecode <$> LB.readFile (toFilePath lf)
  case mErrOrLogins of
    Nothing -> pure Nothing
    Just (Left err) -> do
      putStrLn $ unwords ["Failed to load logins from", fromAbsFile lf, "with error:", err]
      pure Nothing
    Just (Right r) -> pure $ Just r

writeLogins :: Map Username Token -> IO ()
writeLogins m = do
  lf <- loginsFile
  LB.writeFile (toFilePath lf) (JSON.encodePretty m)

instance FromJSON Token where
  parseJSON =
    withText "Token" $ \t ->
      case Base16.decode $ TE.encodeUtf8 t of
        (h, "") -> pure $ Token h
        _ -> fail "Invalid token in JSON: could not decode from hex string"

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

runClientSafe :: ClientM a -> Handler (Either ClientError a)
runClientSafe func = do
  man <- getsYesod appHttpManager
  burl <- getsYesod appAPIBaseUrl
  let cenv = ClientEnv man burl Nothing
  liftIO $ runClientM func cenv

runClientOrErr :: ClientM a -> Handler a
runClientOrErr func = do
  errOrRes <- runClientSafe func
  case errOrRes of
    Left err -> handleStandardServantErrs err $ \resp -> error $ show resp -- TODO deal with error
    Right r -> pure r

runClientOrDisallow :: ClientM a -> Handler (Maybe a)
runClientOrDisallow func = do
  errOrRes <- runClientSafe func
  case errOrRes of
    Left err ->
      handleStandardServantErrs err $ \resp ->
        if responseStatusCode resp == Http.unauthorized401
          then pure Nothing
          else error $ show resp -- TODO deal with error
    Right r -> pure $ Just r
