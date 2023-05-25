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
import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.DirForest (DirForest)
import Data.List (find)
import Data.SemVer as Version
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.Word
import GHC.Generics
import qualified ICal
import Lens.Micro
import qualified Network.HTTP.Types as HTTP
import Path
import Servant.API.Generic
import Servant.Auth.Client
import Servant.Auth.Server
import Servant.Client.Generic
import Servant.Client.Streaming
import Smos.API as X
import Smos.Data hiding (Header)
import Smos.Report.Agenda
import Smos.Report.Next
import System.Exit
import Text.Show.Pretty
import Web.Cookie

smosClient :: APIRoutes (AsClientT ClientM)
smosClient = genericClient

smosUnprotectedClient :: UnprotectedRoutes (AsClientT ClientM)
smosUnprotectedClient = fromServant $ unprotectedRoutes smosClient

clientGetApiVersion :: ClientM Version
clientGetApiVersion = getApiVersion smosUnprotectedClient

clientGetMonetisation :: ClientM (Maybe Monetisation)
clientGetMonetisation = getMonetisation smosUnprotectedClient

clientPostRegister :: Register -> ClientM NoContent
clientPostRegister = postRegister smosUnprotectedClient

clientPostLogin :: Login -> ClientM (Headers '[Header "Set-Cookie" T.Text] NoContent)
clientPostLogin = postLogin smosUnprotectedClient

-- | Get booking settings, fail when booking is not configured or the user does not exist.
clientGetBookingSettings :: Username -> ClientM BookingSettings
clientGetBookingSettings = getBookingSettings smosUnprotectedClient

-- | Get booking settings, 'Nothing' when booking is not configured or the user does not exist.
clientGetBookingSettingsMaybe :: Username -> ClientM (Maybe BookingSettings)
clientGetBookingSettingsMaybe username =
  (Just <$> clientGetBookingSettings username)
    `catchError` ( \err -> case err of
                     FailureResponse _ response ->
                       if responseStatusCode response == HTTP.notFound404
                         then pure Nothing
                         else throwError err
                     _ -> throwError err
                 )

clientGetBookingSlots :: Username -> Word8 -> ClientM BookingSlots
clientGetBookingSlots = getBookingSlots smosUnprotectedClient

clientPostBooking :: Username -> Booking -> ClientM ICal.ICalendar
clientPostBooking = postBooking smosUnprotectedClient

oldestSupportedAPIVersion :: Version
oldestSupportedAPIVersion = version 0 0 0 [] []

-- | Update this to a newer version than the current to build in forward-compatibility
newestSupportedAPIVersion :: Version
newestSupportedAPIVersion = version 0 4 0 [] []

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

clientWithVersionCheck :: ClientM a -> ClientM a
clientWithVersionCheck func = do
  clientEnv <- ask
  withClientVersionCheck clientEnv func

withClientVersionCheck :: MonadIO m => ClientEnv -> m a -> m a
withClientVersionCheck cenv func = do
  errOrTup <- liftIO $ runClientM clientVersionCheck cenv
  case errOrTup of
    Left err ->
      let errOut = liftIO $
            die $
              unlines $
                ("Unable to contact the smos-server to perform an api version check." :) $
                  case err of
                    FailureResponse req resp ->
                      [ "Something went wrong with this request:",
                        ppShow req,
                        "Failure response:",
                        ppShow resp
                      ]
                    ConnectionError e ->
                      [ "There was a problem with the connection, no response received:",
                        displayException e
                      ]
                    e -> ["Unknown problem:", ppShow e]
       in case err of
            FailureResponse _ resp ->
              -- API version 0.0.0, before we had the version check endpoint,
              -- would return a 404 for this endpoint, so if we get a 404 we assume
              -- that the server serves version 0.0.0 of the API.
              if responseStatusCode resp == HTTP.notFound404
                then
                  if oldestSupportedAPIVersion ^. Version.major == 0
                    then func
                    else errOut
                else errOut
            _ -> errOut
    Right (serverVersion, check) ->
      case check of
        OlderThanSupportedMajor ->
          liftIO $
            die $
              unlines $
                [ "The server API version is older than this client supports.",
                  "Interacting with this version is no longer supported in this version of your client.",
                  "Downgrade your client or upgrade your server to interact with it.",
                  ""
                ]
                  ++ versionsErrorHelp oldestSupportedAPIVersion serverVersion newestSupportedAPIVersion
        -- MAYBE Allow showing warnings when a minor version is too old or new
        OlderThanSupportedMinor -> func
        Supported -> func
        NewerThanSupportedMinor -> func
        NewerThanSupportedMajor ->
          liftIO $
            die $
              unlines $
                [ "The server API version is newer than this client supports.",
                  "Interacting with this version is not supported in this version of your client yet.",
                  "Upgrade your client or downgrade your server to interact with it.",
                  ""
                ]
                  ++ versionsErrorHelp oldestSupportedAPIVersion serverVersion newestSupportedAPIVersion

clientVersionsHelpMessage :: [String]
clientVersionsHelpMessage =
  [ unwords ["Oldest supported Smos server API version:", Version.toString oldestSupportedAPIVersion],
    unwords ["Newest supported Smos server API version:", Version.toString newestSupportedAPIVersion]
  ]

smosProtectedClient :: ProtectedRoutes (AsClientT ClientM)
smosProtectedClient = fromServant $ protectedRoutes smosClient

clientGetUserPermissions :: Token -> ClientM UserPermissions
clientGetUserPermissions = getUserPermissions smosProtectedClient

clientGetUserSubscription :: Token -> ClientM SubscriptionStatus
clientGetUserSubscription = getUserSubscription smosProtectedClient

clientPostInitiateStripeCheckoutSession :: Token -> InitiateStripeCheckoutSession -> ClientM InitiatedCheckoutSession
clientPostInitiateStripeCheckoutSession = postInitiateStripeCheckoutSession smosProtectedClient

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
smosReportsClient = fromServant $ reportRoutes smosProtectedClient

clientGetNextActionReport :: Token -> ClientM NextActionReport
clientGetNextActionReport = getNextActionReport smosReportsClient

clientGetAgendaReport :: Token -> ClientM AgendaReport
clientGetAgendaReport = getAgendaReport smosReportsClient

smosAdminClient :: AdminRoutes (AsClientT ClientM)
smosAdminClient = fromServant $ adminRoutes smosClient

clientPostMigrateFiles :: Token -> ClientM NoContent
clientPostMigrateFiles = postMigrateFiles smosAdminClient

clientGetUsers :: Token -> ClientM [UserInfo]
clientGetUsers = getUsers smosAdminClient

clientGetUser :: Token -> Username -> ClientM UserInfo
clientGetUser = getUser smosAdminClient

clientPutUserSubscription :: Token -> Username -> UTCTime -> ClientM NoContent
clientPutUserSubscription = putUserSubscription smosAdminClient

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
