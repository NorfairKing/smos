{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Client
  ( module Smos.Client,
    module X,
    Token,
    SetCookie,
    ClientEnv,
    mkClientEnv,
  )
where

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
import Servant.API.Flatten
import Servant.Auth.Client
import Servant.Auth.Server
import Servant.Client
import Smos.API as X
import Smos.Data hiding (Header)
import Smos.Report.Agenda
import Smos.Report.Next
import System.Exit
import Web.Cookie

clientGetApiVersion :: ClientM Version
clientPostRegister :: Register -> ClientM NoContent
clientPostLogin ::
  Login ->
  ClientM (Headers '[Header "Set-Cookie" T.Text] NoContent)
clientGetApiVersion
  :<|> clientPostRegister
  :<|> clientPostLogin = client (flatten smosUnprotectedAPI)

oldestSupportedAPIVersion :: Version
oldestSupportedAPIVersion = version 0 0 0 [] []

newestSupportedAPIVersion :: Version
newestSupportedAPIVersion = version 0 0 0 [] []

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

clientPostSync :: Token -> SyncRequest -> ClientM SyncResponse
clientGetListBackups :: Token -> ClientM [Backup]
clientGetBackup :: Token -> BackupUUID -> ClientM (SourceIO ByteString)
clientPutRestoreBackup :: Token -> BackupUUID -> ClientM NoContent
clientGetListSmosFiles :: Token -> ClientM (DirForest SmosFile)
clientGetSmosFile :: Token -> Path Rel File -> ClientM SmosFile
clientPutSmosFile :: Token -> Path Rel File -> SmosFile -> ClientM NoContent
clientGetNextActionReport :: Token -> ClientM NextActionReport
clientGetAgendaReport :: Token -> ClientM AgendaReport
clientPostSync
  :<|> clientGetListBackups
  :<|> clientPostBackup
  :<|> clientGetBackup
  :<|> clientPutRestoreBackup
  :<|> clientGetListSmosFiles
  :<|> clientGetSmosFile
  :<|> clientPutSmosFile
  :<|> clientGetNextActionReport
  :<|> clientGetAgendaReport = client (flatten smosProtectedAPI)

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

runClient :: ClientEnv -> ClientM a -> IO (Either ClientError a)
runClient = flip runClientM

runClientOrDie :: ClientEnv -> ClientM a -> IO a
runClientOrDie cenv func = do
  errOrResp <- runClient cenv func
  case errOrResp of
    Left err -> liftIO $ die $ show err
    Right resp -> pure resp
