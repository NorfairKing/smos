{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.API
  ( module Smos.API,
    module X,
  )
where

import Autodocodec
import Control.Arrow
import Control.DeepSeq
import Control.Exception
import Data.Aeson as JSON (FromJSON, ToJSON, Value)
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
apiVersion = version 0 2 1 [] []

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
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AuthNCookie)

instance HasCodec AuthNCookie where
  codec =
    object "AuthNCookie" $
      AuthNCookie
        <$> requiredField "username" "username" .= authNCookieUsername

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
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Monetisation)

instance Validity Monetisation

instance NFData Monetisation

instance HasCodec Monetisation where
  codec =
    object "Monetisation" $
      Monetisation
        <$> requiredField "publishable-key" "Stripe publishable key" .= monetisationStripePublishableKey
        <*> requiredField "currency" "currency of the price" .= monetisationStripePriceCurrency
        <*> requiredField "price-per-year" "price per year, in minimal quantisations" .= monetisationStripePricePerYear

type PostRegister = "register" :> ReqBody '[JSON] Register :> PostNoContent '[JSON] NoContent

data Register = Register
  { registerUsername :: Username,
    registerPassword :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Register)

instance Validity Register

instance NFData Register

instance HasCodec Register where
  codec =
    object "Register" $
      Register
        <$> parseAlternative
          (requiredField "registerUsername" "legacy key")
          (requiredField "username" "username")
          .= registerUsername
        <*> parseAlternative
          (requiredField "registerPassword" "legacy key")
          (requiredField "password" "password")
          .= registerPassword

type PostLogin =
  "login"
    :> ReqBody '[JSON] Login
    :> PostNoContent '[JSON] (Headers '[Header "Set-Cookie" T.Text] NoContent)

data Login = Login
  { loginUsername :: Username,
    loginPassword :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Login)

instance Validity Login

instance NFData Login

instance HasCodec Login where
  codec =
    object "Login" $
      Login
        <$> parseAlternative (requiredField "loginUsername" "legacy key") (requiredField "username" "username") .= loginUsername
        <*> parseAlternative (requiredField "loginPassword" "legacy key") (requiredField "password" "password") .= loginPassword

type PostStripeHook = "stripe" :> ReqBody '[JSON] JSON.Value :> PostNoContent '[JSON] NoContent

type GetUserPermissions = "user" :> "permissions" :> Get '[JSON] UserPermissions

data UserPermissions = UserPermissions
  { userPermissionsIsAdmin :: Bool
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec UserPermissions)

instance Validity UserPermissions

instance NFData UserPermissions

instance HasCodec UserPermissions where
  codec =
    object "UserPermissions" $
      UserPermissions
        <$> optionalFieldWithDefault "admin" False "whether the user is an admin" .= userPermissionsIsAdmin

type GetUserSubscription = "user" :> "subscription" :> Get '[JSON] SubscriptionStatus

data SubscriptionStatus
  = NoSubscriptionNecessary
  | SubscribedUntil UTCTime
  | NotSubscribed
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec SubscriptionStatus)

instance Validity SubscriptionStatus

instance NFData SubscriptionStatus

instance HasCodec SubscriptionStatus where
  codec =
    dimapCodec f g $
      object "SubscriptionStatus" $
        eitherCodec noSubscriptionNecessaryCodec $
          eitherCodec subscribedUntilCodec notSubscribedCodec
    where
      f = \case
        Left () -> NoSubscriptionNecessary
        Right (Left u) -> SubscribedUntil u
        Right (Right ()) -> NotSubscribed
      g = \case
        NoSubscriptionNecessary -> Left ()
        SubscribedUntil u -> Right (Left u)
        NotSubscribed -> Right (Right ())

      noSubscriptionNecessaryCodec :: JSONObjectCodec ()
      noSubscriptionNecessaryCodec =
        dimapCodec (const ()) (const "") $
          requiredFieldWith "status" (literalTextCodec "no-subscription-necessary") "status: no subscription necessary"
      subscribedUntilCodec :: JSONObjectCodec UTCTime
      subscribedUntilCodec =
        dimapCodec snd (\u -> ("", u)) $
          (,)
            <$> requiredFieldWith "status" (literalTextCodec "subscribed") "status: subscribed" .= fst
            <*> requiredField "until" "until when the subscription is active" .= snd

      notSubscribedCodec :: JSONObjectCodec ()
      notSubscribedCodec =
        dimapCodec (const ()) (const "") $
          requiredFieldWith "status" (literalTextCodec "not-subscribed") "status: not subscribed"

type PostInitiateStripeCheckoutSession = "checkout" :> "stripe" :> "session" :> ReqBody '[JSON] InitiateStripeCheckoutSession :> Post '[JSON] InitiatedCheckoutSession

data InitiateStripeCheckoutSession = InitiateStripeCheckoutSession
  { initiateStripeCheckoutSessionSuccessUrl :: Text,
    initiateStripeCheckoutSessionCanceledUrl :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec InitiateStripeCheckoutSession)

instance Validity InitiateStripeCheckoutSession

instance NFData InitiateStripeCheckoutSession

instance HasCodec InitiateStripeCheckoutSession where
  codec =
    object "InitiateStripeCheckoutSession" $
      InitiateStripeCheckoutSession
        <$> requiredField "success" "success url" .= initiateStripeCheckoutSessionSuccessUrl
        <*> requiredField "canceled" "canceled url" .= initiateStripeCheckoutSessionCanceledUrl

data InitiatedCheckoutSession = InitiatedCheckoutSession
  { initiatedCheckoutSessionId :: Text,
    initiatedCheckoutSessionCustomerId :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec InitiatedCheckoutSession)

instance Validity InitiatedCheckoutSession

instance NFData InitiatedCheckoutSession

instance HasCodec InitiatedCheckoutSession where
  codec =
    object "InitiatedCheckoutSession" $
      InitiatedCheckoutSession
        <$> requiredField "session" "session identifier" .= initiatedCheckoutSessionId
        <*> optionalField "customer" "customer identifier" .= initiatedCheckoutSessionCustomerId

type DeleteUser = "user" :> DeleteNoContent '[JSON] NoContent

type PostSync = "sync" :> ReqBody '[JSON] SyncRequest :> Post '[JSON] SyncResponse

data BackupInfo = BackupInfo
  { backupInfoUUID :: !BackupUUID,
    backupInfoTime :: !UTCTime,
    backupInfoSize :: !Word64
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec BackupInfo)

instance Validity BackupInfo

instance NFData BackupInfo

instance HasCodec BackupInfo where
  codec =
    object "BackupInfo" $
      BackupInfo
        <$> requiredField "uuid" "backup identifier" .= backupInfoUUID
        <*> requiredField "time" "backup timestamp" .= backupInfoTime
        <*> requiredField "size" "total size, in bytes" .= backupInfoSize

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
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec SyncFile)

instance Validity SyncFile

instance NFData SyncFile

instance HasCodec SyncFile where
  codec =
    object "SyncFile" $
      SyncFile
        <$> requiredFieldWith
          "contents"
          ( bimapCodec
              (Base64.decode . SB8.pack)
              (SB8.unpack . Base64.encode)
              codec
          )
          "file contents, base64 encoded"
          .= syncFileContents

data SyncRequest = SyncRequest
  { syncRequestItems :: Mergeful.SyncRequest (Path Rel File) (Path Rel File) SyncFile
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec SyncRequest)

instance Validity SyncRequest

instance NFData SyncRequest

instance HasCodec SyncRequest where
  codec =
    object "SyncRequest" $
      SyncRequest
        <$> requiredFieldWith "items" (codecViaAeson "Mergeful.SyncRequest") "sync request of the smos files" .= syncRequestItems

data SyncResponse = SyncResponse
  { syncResponseServerId :: ServerUUID,
    syncResponseItems :: Mergeful.SyncResponse (Path Rel File) (Path Rel File) SyncFile
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec SyncResponse)

instance Validity SyncResponse

instance NFData SyncResponse

instance HasCodec SyncResponse where
  codec =
    object "SyncResponse" $
      SyncResponse
        <$> requiredField "server-id" "identifier of the server" .= syncResponseServerId
        <*> requiredFieldWith "items" (codecViaAeson "Mergeful.SyncResponse") "sync response of the smos files" .= syncResponseItems

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
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec UserInfo)

instance Validity UserInfo

instance NFData UserInfo

instance HasCodec UserInfo where
  codec =
    object "UserInfo" $
      UserInfo
        <$> requiredField "name" "user name" .= userInfoUsername
        <*> optionalFieldWithDefault "admin" False "whether the user is an admin" .= userInfoAdmin
        <*> requiredField "created" "user creation time" .= userInfoCreated
        <*> requiredField "last-login" "last time the user logged in" .= userInfoLastLogin
        <*> requiredField "last-use" "last time the user used the API" .= userInfoLastUse
        <*> requiredField "subscribed" "user subscription status" .= userInfoSubscribed

type PutUserSubscription = "users" :> Capture "username" Username :> ReqBody '[JSON] UTCTime :> PutNoContent '[JSON] NoContent
