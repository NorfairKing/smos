{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Client
  ( module Smos.Client,
    module X,
    BaseUrl (..),
    showBaseUrl,
    Scheme (..),
    ClientM,
    ClientError (..),
    Token,
    SetCookie,
    ClientEnv,
    mkClientEnv,
    Response,
    ResponseF (..),
  )
where

import Control.DeepSeq
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.DirForest (DirForest)
import Data.List (find)
import Data.SemVer as Version
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Lens.Micro
import qualified Network.HTTP.Types as HTTP
import Path
import Servant.Auth.Client
import Servant.Auth.Server
import Servant.Client.Generic
import Servant.Client.Streaming
import Smos.API as X
import Smos.Data hiding (Header)
import Smos.Report.Agenda
import Smos.Report.Next
import System.Exit
import Web.Cookie

smosUnprotectedClient :: UnprotectedRoutes (AsClientT ClientM)
smosUnprotectedClient = genericClient

clientGetApiVersion :: ClientM Version
clientGetApiVersion = getApiVersion smosUnprotectedClient

clientPostRegister :: Register -> ClientM NoContent
clientPostRegister = postRegister smosUnprotectedClient

clientPostLogin :: Login -> ClientM (Headers '[Header "Set-Cookie" T.Text] NoContent)
clientPostLogin = postLogin smosUnprotectedClient

oldestSupportedAPIVersion :: Version
oldestSupportedAPIVersion = version 0 0 0 [] []

newestSupportedAPIVersion :: Version
newestSupportedAPIVersion = version 0 1 0 [] []

clientVersionCheck :: ClientM (Version, VersionCheck)
clientVersionCheck = do
  serverVersion <- clientGetApiVersion
  pure
    ( serverVersion,
      versionCheck -- A version check of whether the server version is supported from the client's perspective
        oldestSupportedAPIVersion
        newestSupportedAPIVersion
        serverVersion
    )

clientDoVersionCheck :: ClientM (Either String ())
clientDoVersionCheck = do
  (serverVersion, check) <- clientVersionCheck
  pure $ case check of
    OlderThanSupported ->
      Left $
        unlines $
          [ "The server API version is older than this client supports.",
            "Interacting with this version is no longer supported in this version of your client.",
            "Downgrade your client or upgrade your server to interact with it.",
            ""
          ]
            ++ versionsErrorHelp oldestSupportedAPIVersion serverVersion newestSupportedAPIVersion
    NewerThanSupported ->
      Left $
        unlines $
          [ "The server API version is newer than this client supports.",
            "Interacting with this version is not supported in this version of your client yet.",
            "Upgrade your client or downgrade your server to interact with it.",
            ""
          ]
            ++ versionsErrorHelp oldestSupportedAPIVersion serverVersion newestSupportedAPIVersion
    Supported -> Right ()

clientWithVersionCheck :: ClientM a -> ClientM a
clientWithVersionCheck func = do
  errOrUnit <- clientDoVersionCheck
  case errOrUnit of
    Left err -> liftIO $ die err
    Right () -> func

withClientVersionCheck :: MonadIO m => ClientEnv -> m a -> m a
withClientVersionCheck cenv func = do
  errOrUnit <- liftIO $ runClientM clientDoVersionCheck cenv
  case errOrUnit of
    Left err ->
      let errOut = liftIO $ die $ unlines ["Unable to contact the smos-server to perform an api version check:", show err]
       in case err of
            FailureResponse _ resp ->
              if responseStatusCode resp == HTTP.notFound404 -- API version 0.0.0, before we had the version check endpoint
                then
                  if oldestSupportedAPIVersion ^. Version.major == 0
                    then func
                    else errOut
                else errOut
            _ -> errOut
    Right (Left err) -> liftIO $ die err
    Right (Right ()) -> func

clientVersionsHelpMessage :: [String]
clientVersionsHelpMessage =
  [ unwords ["Oldest supported Smos server API version:", Version.toString oldestSupportedAPIVersion],
    unwords ["Newest supported Smos server API version:", Version.toString newestSupportedAPIVersion]
  ]

smosProtectedClient :: ProtectedRoutes (AsClientT ClientM)
smosProtectedClient = genericClient

clientGetUserPermissions :: Token -> ClientM UserPermissions
clientGetUserPermissions = getUserPermissions smosProtectedClient

clientDeleteUser :: Token -> ClientM NoContent
clientDeleteUser = deleteUser smosProtectedClient

clientPostSync :: Token -> SyncRequest -> ClientM SyncResponse
clientPostSync = postSync smosProtectedClient

clientGetListBackups :: Token -> ClientM [BackupInfo]
clientGetListBackups = getListBackups smosProtectedClient

clientGetBackup :: Token -> BackupUUID -> ClientM (SourceIO ByteString)
clientGetBackup = getBackup smosProtectedClient

clientPostBackup :: Token -> ClientM BackupUUID
clientPostBackup = postBackup smosProtectedClient

clientPutRestoreBackup :: Token -> BackupUUID -> ClientM NoContent
clientPutRestoreBackup = putRestoreBackup smosProtectedClient

clientDeleteBackup :: Token -> BackupUUID -> ClientM NoContent
clientDeleteBackup = deleteBackup smosProtectedClient

clientGetListSmosFiles :: Token -> ClientM (DirForest SmosFile)
clientGetListSmosFiles = getListSmosFiles smosProtectedClient

clientGetSmosFile :: Token -> Path Rel File -> ClientM SmosFile
clientGetSmosFile = getSmosFile smosProtectedClient

clientPutSmosFile :: Token -> Path Rel File -> SmosFile -> ClientM NoContent
clientPutSmosFile = putSmosFile smosProtectedClient

smosReportsClient :: ReportRoutes (AsClientT ClientM)
smosReportsClient = genericClient

clientGetNextActionReport :: Token -> ClientM NextActionReport
clientGetNextActionReport = getNextActionReport smosReportsClient

clientGetAgendaReport :: Token -> ClientM AgendaReport
clientGetAgendaReport = getAgendaReport smosReportsClient

smosAdminClient :: AdminRoutes (AsClientT ClientM)
smosAdminClient = genericClient

clientGetUsers :: Token -> ClientM [UserInfo]
clientGetUsers = getUsers smosAdminClient

clientLogin :: Login -> ClientM (Either HeaderProblem Token)
clientLogin = fmap (fmap sessionToToken) . clientLoginSession

clientLoginSession :: Login -> ClientM (Either HeaderProblem SetCookie)
clientLoginSession lf = do
  res <- clientPostLogin lf
  pure $
    case res of
      Headers NoContent (HCons sessionHeader HNil) ->
        case sessionHeader of
          MissingHeader -> Left ProblemMissingHeader
          UndecodableHeader b -> Left $ ProblemUndecodableHeader b
          Header sessionText ->
            let cookies =
                  parseSetCookie . encodeUtf8 <$> T.lines sessionText
                jwtCookie =
                  find ((== "JWT-Cookie") . setCookieName) cookies
             in case jwtCookie of
                  Nothing -> Left ProblemMissingJWTCookie
                  Just setCookie -> Right setCookie

sessionToToken :: SetCookie -> Token
sessionToToken = Token . setCookieValue

data HeaderProblem
  = ProblemMissingHeader
  | ProblemUndecodableHeader ByteString
  | ProblemMissingJWTCookie
  deriving (Show, Eq, Generic)

instance NFData HeaderProblem

instance NFData Token

login :: ClientEnv -> Login -> IO (Either LoginError Token)
login cenv lf = do
  errOrRes <- runClient cenv $ clientLogin lf
  pure $
    case errOrRes of
      Left se -> Left $ LoginServantError se
      Right (Left hp) -> Left $ LoginHeaderProblem hp
      Right (Right t) -> Right t

data LoginError
  = LoginServantError ClientError
  | LoginHeaderProblem HeaderProblem
  deriving (Show, Eq, Generic)

runClient :: NFData a => ClientEnv -> ClientM a -> IO (Either ClientError a)
runClient = flip runClientM

runClientOrDie :: ClientEnv -> ClientM a -> IO a
runClientOrDie cenv func = withClientM func cenv $ \case
  Left err -> liftIO $ die $ show err
  Right resp -> pure resp
