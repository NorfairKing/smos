{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.API
  ( module Smos.API,
    module X,
  )
where

import Control.Arrow
import Control.DeepSeq
import Control.Exception
import Data.Aeson as JSON
import Data.ByteString (ByteString)
import Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import Data.DirForest (DirForest (..))
import qualified Data.Mergeful as Mergeful
import Data.Mergeful.Persistent ()
import Data.Proxy
import Data.SemVer as Version
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import qualified Data.UUID as UUID
import Data.UUID.Typed as UUID
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Path ()
import Data.Validity.Text ()
import Data.Validity.UUID ()
import Data.Word
import Database.Persist
import Database.Persist.Sql
import Path
import Path.Internal
import Servant.API as X
import Servant.API.Generic
import Servant.Auth
import Servant.Auth.Server
import Smos.API.Password as X
import Smos.API.SHA256 as X
import Smos.API.Username as X
import Smos.Data hiding (Header)
import Smos.Report.Agenda
import Smos.Report.Next

apiVersion :: Version
apiVersion = version 0 2 0 [] []

smosAPI :: Proxy SmosAPI
smosAPI = Proxy

type SmosAPI = ToServantApi APIRoutes

data APIRoutes route = APIRoutes
  { unprotectedRoutes :: route :- ToServantApi UnprotectedRoutes,
    protectedRoutes :: route :- ToServantApi ProtectedRoutes,
    adminRoutes :: route :- "admin" :> ToServantApi AdminRoutes
  }
  deriving (Generic)

smosUnprotectedAPI :: Proxy SmosUnprotectedAPI
smosUnprotectedAPI = Proxy

type SmosUnprotectedAPI = ToServantApi UnprotectedRoutes

data UnprotectedRoutes route = UnprotectedRoutes
  { getApiVersion :: !(route :- GetAPIVersion),
    getMonetisation :: !(route :- GetMonetisation),
    postRegister :: !(route :- PostRegister),
    postLogin :: !(route :- PostLogin),
    postStripeHook :: !(route :- PostStripeHook)
  }
  deriving (Generic)

smosProtectedAPI :: Proxy SmosProtectedAPI
smosProtectedAPI = Proxy

type ProtectAPI = Auth '[JWT] AuthNCookie

data AuthNCookie = AuthNCookie
  { authNCookieUsername :: Username
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AuthNCookie where
  parseJSON = withObject "AuthNCookie" $ \o ->
    AuthNCookie
      <$> o .: "username"

instance ToJSON AuthNCookie where
  toJSON AuthNCookie {..} =
    object
      [ "username" .= authNCookieUsername
      ]

instance FromJWT AuthNCookie

instance ToJWT AuthNCookie

type SmosProtectedAPI = ToServantApi ProtectedRoutes

data ProtectedRoutes route = ProtectedRoutes
  { getUserPermissions :: !(route :- ProtectAPI :> GetUserPermissions),
    getUserSubscription :: !(route :- ProtectAPI :> GetUserSubscription),
    postInitiateStripeCheckoutSession :: !(route :- ProtectAPI :> PostInitiateStripeCheckoutSession),
    deleteUser :: !(route :- ProtectAPI :> DeleteUser),
    postSync :: !(route :- ProtectAPI :> PostSync),
    getListBackups :: !(route :- ProtectAPI :> GetListBackups),
    postBackup :: !(route :- ProtectAPI :> PostBackup),
    getBackup :: !(route :- ProtectAPI :> GetBackup),
    putRestoreBackup :: !(route :- ProtectAPI :> PutRestoreBackup),
    deleteBackup :: !(route :- ProtectAPI :> DeleteBackup),
    getListSmosFiles :: !(route :- ProtectAPI :> GetListSmosFiles),
    getSmosFile :: !(route :- ProtectAPI :> GetSmosFile),
    putSmosFile :: !(route :- ProtectAPI :> PutSmosFile),
    reportRoutes :: !(route :- "report" :> ToServantApi ReportRoutes)
  }
  deriving (Generic)

type GetAPIVersion = "api-version" :> Get '[JSON] Version

type GetMonetisation = "monetisation" :> Get '[JSON] (Maybe Monetisation)

data Monetisation = Monetisation
  { monetisationStripePublishableKey :: !Text,
    monetisationStripePriceCurrency :: !Text,
    monetisationStripePricePerYear :: !Int
  }
  deriving (Show, Eq, Generic)

instance Validity Monetisation

instance NFData Monetisation

instance FromJSON Monetisation where
  parseJSON = withObject "Monetisation" $ \o ->
    Monetisation
      <$> o .: "publishable-key"
      <*> o .: "currency"
      <*> o .: "price-per-year"

instance ToJSON Monetisation where
  toJSON Monetisation {..} =
    object
      [ "publishable-key" .= monetisationStripePublishableKey,
        "currency" .= monetisationStripePriceCurrency,
        "price-per-year" .= monetisationStripePricePerYear
      ]

type PostRegister = "register" :> ReqBody '[JSON] Register :> PostNoContent '[JSON] NoContent

data Register = Register
  { registerUsername :: Username,
    registerPassword :: Text
  }
  deriving (Show, Eq, Generic)

instance Validity Register

instance NFData Register

instance ToJSON Register

instance FromJSON Register

type PostLogin =
  "login"
    :> ReqBody '[JSON] Login
    :> PostNoContent '[JSON] (Headers '[Header "Set-Cookie" T.Text] NoContent)

data Login = Login
  { loginUsername :: Username,
    loginPassword :: Text
  }
  deriving (Show, Eq, Generic)

instance Validity Login

instance NFData Login

instance ToJSON Login

instance FromJSON Login

type PostStripeHook = "stripe" :> ReqBody '[JSON] JSON.Value :> PostNoContent '[JSON] NoContent

type GetUserPermissions = "user" :> "permissions" :> Get '[JSON] UserPermissions

data UserPermissions = UserPermissions
  { userPermissionsIsAdmin :: Bool
  }
  deriving (Show, Eq, Generic)

instance Validity UserPermissions

instance NFData UserPermissions

instance FromJSON UserPermissions where
  parseJSON = withObject "UserPermissions" $ \o ->
    UserPermissions
      <$> o .:? "admin" .!= False

instance ToJSON UserPermissions where
  toJSON UserPermissions {..} =
    object
      [ "admin" .= userPermissionsIsAdmin
      ]

type GetUserSubscription = "user" :> "subscription" :> Get '[JSON] SubscriptionStatus

data SubscriptionStatus = NoSubscriptionNecessary | SubscribedUntil UTCTime | NotSubscribed
  deriving (Show, Eq, Generic)

instance Validity SubscriptionStatus

instance NFData SubscriptionStatus

instance FromJSON SubscriptionStatus where
  parseJSON = withObject "UserSubscription" $ \o -> do
    t <- o .: "status"
    case (t :: Text) of
      "not-subscribed" -> pure NotSubscribed
      "subscribed" -> SubscribedUntil <$> o .: "until"
      "no-subscription-necessary" -> pure NoSubscriptionNecessary
      _ -> fail "Unknown SubscriptionStatus"

instance ToJSON SubscriptionStatus where
  toJSON =
    let o t vs = object $ ("status" .= (t :: Text)) : vs
     in \case
          NotSubscribed -> o "not-subscribed" []
          SubscribedUntil ut -> o "subscribed" ["until" .= ut]
          NoSubscriptionNecessary -> o "no-subscription-necessary" []

type PostInitiateStripeCheckoutSession = "checkout" :> "stripe" :> "session" :> ReqBody '[JSON] InitiateStripeCheckoutSession :> Post '[JSON] InitiatedCheckoutSession

data InitiateStripeCheckoutSession = InitiateStripeCheckoutSession
  { initiateStripeCheckoutSessionSuccessUrl :: Text,
    initiateStripeCheckoutSessionCanceledUrl :: Text
  }
  deriving (Show, Eq, Generic)

instance Validity InitiateStripeCheckoutSession

instance NFData InitiateStripeCheckoutSession

instance FromJSON InitiateStripeCheckoutSession where
  parseJSON = withObject "InitiateStripeCheckoutSession" $ \o ->
    InitiateStripeCheckoutSession
      <$> o .: "success"
      <*> o .: "canceled"

instance ToJSON InitiateStripeCheckoutSession where
  toJSON InitiateStripeCheckoutSession {..} =
    object
      [ "success" .= initiateStripeCheckoutSessionSuccessUrl,
        "canceled" .= initiateStripeCheckoutSessionCanceledUrl
      ]

data InitiatedCheckoutSession = InitiatedCheckoutSession
  { initiatedCheckoutSessionId :: Text,
    initiatedCheckoutSessionCustomerId :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance Validity InitiatedCheckoutSession

instance NFData InitiatedCheckoutSession

instance FromJSON InitiatedCheckoutSession where
  parseJSON = withObject "InitiatedCheckoutSession" $ \o ->
    InitiatedCheckoutSession <$> o .: "session" <*> o .:? "customer"

instance ToJSON InitiatedCheckoutSession where
  toJSON InitiatedCheckoutSession {..} =
    object
      [ "session" .= initiatedCheckoutSessionId,
        "customer" .= initiatedCheckoutSessionCustomerId
      ]

type DeleteUser = "user" :> DeleteNoContent '[JSON] NoContent

type PostSync = "sync" :> ReqBody '[JSON] SyncRequest :> Post '[JSON] SyncResponse

data BackupInfo = BackupInfo
  { backupInfoUUID :: !BackupUUID,
    -- | When the backup was made
    backupInfoTime :: !UTCTime,
    -- | In bytes
    backupInfoSize :: !Word64
  }
  deriving (Show, Eq, Generic)

instance Validity BackupInfo

instance NFData BackupInfo

instance FromJSON BackupInfo where
  parseJSON = withObject "Backup" $ \o ->
    BackupInfo
      <$> o .: "uuid"
      <*> o .: "time"
      <*> o .: "size"

instance ToJSON BackupInfo where
  toJSON BackupInfo {..} =
    object
      [ "uuid" .= backupInfoUUID,
        "time" .= backupInfoTime,
        "size" .= backupInfoSize
      ]

type BackupUUID = UUID BackupInfo

type GetListBackups = "backups" :> Get '[JSON] [BackupInfo]

type PostBackup = "backup" :> Post '[JSON] BackupUUID

type GetBackup = "backup" :> Capture "backup" BackupUUID :> StreamGet NoFraming OctetStream (SourceIO ByteString)

type PutRestoreBackup = "backup" :> Capture "backup" BackupUUID :> "restore" :> PutNoContent '[JSON] NoContent

type DeleteBackup = "backup" :> Capture "backup" BackupUUID :> DeleteNoContent '[JSON] NoContent

data SyncServer

type ServerUUID = UUID SyncServer

deriving instance PersistFieldSql (Path Rel File) -- TODO Not entirely safe

deriving instance PersistField (Path Rel File) -- TODO Not entirely safe

instance PersistField (UUID a) where
  toPersistValue (UUID uuid) = PersistByteString $ LB.toStrict $ UUID.toByteString uuid
  fromPersistValue (PersistByteString bs) =
    case UUID.fromByteString $ LB.fromStrict bs of
      Nothing -> Left "Invalidy Bytestring to convert to UUID"
      Just uuid -> Right $ UUID uuid
  fromPersistValue pv = Left $ "Invalid Persist value to parse to UUID: " <> T.pack (show pv)

instance PersistFieldSql (UUID a) where
  sqlType Proxy = SqlBlob

newtype SyncFile = SyncFile
  { syncFileContents :: ByteString
  }
  deriving (Show, Eq, Generic)

instance Validity SyncFile

instance NFData SyncFile

instance FromJSON SyncFile where
  parseJSON =
    withObject "SyncFile" $ \o ->
      SyncFile
        <$> ( do
                base64Contents <- SB8.pack <$> o .: "contents"
                case Base64.decode base64Contents of
                  Left err -> fail err
                  Right r -> pure r
            )

instance ToJSON SyncFile where
  toJSON SyncFile {..} =
    object ["contents" .= SB8.unpack (Base64.encode syncFileContents)]

data SyncRequest = SyncRequest
  { syncRequestItems :: Mergeful.SyncRequest (Path Rel File) (Path Rel File) SyncFile
  }
  deriving (Show, Eq, Generic)

instance Validity SyncRequest

instance NFData SyncRequest

instance FromJSON SyncRequest where
  parseJSON = withObject "SyncRequest" $ \o -> SyncRequest <$> o .: "items"

instance ToJSON SyncRequest where
  toJSON SyncRequest {..} =
    object ["items" .= syncRequestItems]

data SyncResponse = SyncResponse
  { syncResponseServerId :: ServerUUID,
    syncResponseItems :: Mergeful.SyncResponse (Path Rel File) (Path Rel File) SyncFile
  }
  deriving (Show, Eq, Generic)

instance Validity SyncResponse

instance NFData SyncResponse

instance FromJSON SyncResponse where
  parseJSON = withObject "SyncResponse" $ \o -> SyncResponse <$> o .: "server-id" <*> o .: "items"

instance ToJSON SyncResponse where
  toJSON SyncResponse {..} =
    object ["server-id" .= syncResponseServerId, "items" .= syncResponseItems]

instance FromHttpApiData (Path Rel File) where
  parseQueryParam t = left (T.pack . displayException :: SomeException -> Text) $ parseRelFile (T.unpack t)

instance ToHttpApiData (Path Rel File) where
  toQueryParam = T.pack . fromRelFile

type GetListSmosFiles = "files" :> Get '[JSON] (DirForest SmosFile)

type GetSmosFile = "file" :> QueryParam' '[Required, Strict] "path" (Path Rel File) :> Get '[JSON] SmosFile

type PutSmosFile = "file" :> QueryParam' '[Required, Strict] "path" (Path Rel File) :> ReqBody '[JSON] SmosFile :> PutNoContent '[JSON] NoContent

type ReportsAPI = ToServantApi ReportRoutes

data ReportRoutes route = ReportRoutes
  { getNextActionReport :: !(route :- ProtectAPI :> GetNextActionReport),
    getAgendaReport :: !(route :- ProtectAPI :> GetAgendaReport)
  }
  deriving (Generic)

type GetNextActionReport = "next" :> Get '[JSON] NextActionReport

type GetAgendaReport = "agenda" :> Get '[JSON] AgendaReport

reportsAPI :: Proxy ReportsAPI
reportsAPI = Proxy

type ProtectAdmin = Auth '[JWT] AuthNCookie

newtype AdminCookie = AdminCookie {adminCookieUsername :: Username}
  deriving (Show, Eq, Ord, Generic)

data AdminRoutes route = AdminRoutes
  { postMigrateFiles :: !(route :- ProtectAPI :> PostMigrateFiles),
    getUsers :: !(route :- ProtectAdmin :> GetUsers),
    getUser :: !(route :- ProtectAdmin :> GetUser),
    putUserSubscription :: !(route :- ProtectAdmin :> PutUserSubscription)
  }
  deriving (Generic)

type PostMigrateFiles = "migrator-files" :> PostNoContent '[JSON] NoContent

type GetUsers = "users" :> Get '[JSON] [UserInfo]

type GetUser = "users" :> Capture "username" Username :> Get '[JSON] UserInfo

data UserInfo = UserInfo
  { userInfoUsername :: !Username,
    userInfoAdmin :: !Bool,
    userInfoCreated :: !UTCTime,
    userInfoLastLogin :: !(Maybe UTCTime),
    userInfoLastUse :: !(Maybe UTCTime),
    userInfoSubscribed :: !SubscriptionStatus
  }
  deriving (Show, Eq, Generic)

instance Validity UserInfo

instance NFData UserInfo

instance ToJSON UserInfo where
  toJSON UserInfo {..} =
    object
      [ "name" .= userInfoUsername,
        "admin" .= userInfoAdmin,
        "created" .= userInfoCreated,
        "last-login" .= userInfoLastLogin,
        "last-use" .= userInfoLastUse,
        "subscribed" .= userInfoSubscribed
      ]

instance FromJSON UserInfo where
  parseJSON = withObject "UserInfo" $ \o ->
    UserInfo
      <$> o .: "name"
      <*> o .: "admin"
      <*> o .: "created"
      <*> o .:? "last-login"
      <*> o .:? "last-use"
      <*> o .:? "subscribed" .!= NotSubscribed

type PutUserSubscription = "users" :> Capture "username" Username :> ReqBody '[JSON] UTCTime :> PutNoContent '[JSON] NoContent
