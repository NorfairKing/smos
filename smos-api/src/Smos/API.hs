{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Data.Map (Map)
import qualified Data.Mergeful as Mergeful
import Data.Mergeful.Persistent ()
import Data.Proxy
import Data.SemVer as Version
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Zones.All
import qualified Data.UUID as UUID
import Data.UUID.Typed as UUID
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Path ()
import Data.Validity.Text ()
import Data.Validity.UUID ()
import Data.Word
import Data.Yaml.Builder (ToYaml)
import Database.Persist
import Database.Persist.Sql
import qualified ICal
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
apiVersion = version 0 4 0 [] []

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
    postStripeHook :: !(route :- PostStripeHook),
    -- Booking
    getBookingSettings :: !(route :- GetBookingSettings),
    getBookingSlots :: !(route :- GetBookingSlots),
    postBooking :: !(route :- PostBooking)
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
        <$> requiredField "username" "username"
          .= authNCookieUsername

instance FromJWT AuthNCookie

instance ToJWT AuthNCookie

type SmosProtectedAPI = ToServantApi ProtectedRoutes

data ProtectedRoutes route = ProtectedRoutes
  { -- Account
    getUserPermissions :: !(route :- ProtectAPI :> GetUserPermissions),
    getUserSubscription :: !(route :- ProtectAPI :> GetUserSubscription),
    postInitiateStripeCheckoutSession :: !(route :- ProtectAPI :> PostInitiateStripeCheckoutSession),
    deleteUser :: !(route :- ProtectAPI :> DeleteUser),
    -- Sync
    postSync :: !(route :- ProtectAPI :> PostSync),
    -- Backups
    getListBackups :: !(route :- ProtectAPI :> GetListBackups),
    postBackup :: !(route :- ProtectAPI :> PostBackup),
    getBackup :: !(route :- ProtectAPI :> GetBackup),
    putRestoreBackup :: !(route :- ProtectAPI :> PutRestoreBackup),
    deleteBackup :: !(route :- ProtectAPI :> DeleteBackup),
    -- Individual files
    getListSmosFiles :: !(route :- ProtectAPI :> GetListSmosFiles),
    getSmosFile :: !(route :- ProtectAPI :> GetSmosFile),
    putSmosFile :: !(route :- ProtectAPI :> PutSmosFile),
    -- Routes
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
        <$> requiredField "publishable-key" "Stripe publishable key"
          .= monetisationStripePublishableKey
        <*> requiredField "currency" "currency of the price"
          .= monetisationStripePriceCurrency
        <*> requiredField "price-per-year" "price per year, in minimal quantisations"
          .= monetisationStripePricePerYear

-- We cannot use PostNoContent because of this issue:
-- https://github.com/haskell-servant/servant-auth/issues/177
-- The suggested workaround there also doesn't seem to work .. (?)
type PostRegister = "register" :> ReqBody '[JSON] Register :> Verb 'POST 204 '[JSON] NoContent

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
          (requiredField "username" "username")
          (requiredField "registerUsername" "legacy key")
          .= registerUsername
        <*> parseAlternative
          (requiredField "password" "password")
          (requiredField "registerPassword" "legacy key")
          .= registerPassword

type PostLogin =
  "login"
    :> ReqBody '[JSON] Login
    :> Verb 'POST 204 '[JSON] (Headers '[Header "Set-Cookie" T.Text] NoContent)

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
        <$> parseAlternative
          (requiredField "username" "username")
          (requiredField "loginUsername" "legacy key")
          .= loginUsername
        <*> parseAlternative
          (requiredField "password" "password")
          (requiredField "loginPassword" "legacy key")
          .= loginPassword

type PostStripeHook = "stripe" :> ReqBody '[JSON] JSON.Value :> Verb 'POST 204 '[JSON] NoContent

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
        <$> optionalFieldWithDefault "admin" False "whether the user is an admin"
          .= userPermissionsIsAdmin

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
            <$> requiredFieldWith "status" (literalTextCodec "subscribed") "status: subscribed"
              .= fst
            <*> requiredField "until" "until when the subscription is active"
              .= snd

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
        <$> requiredField "success" "success url"
          .= initiateStripeCheckoutSessionSuccessUrl
        <*> requiredField "canceled" "canceled url"
          .= initiateStripeCheckoutSessionCanceledUrl

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
        <$> requiredField "session" "session identifier"
          .= initiatedCheckoutSessionId
        <*> optionalField "customer" "customer identifier"
          .= initiatedCheckoutSessionCustomerId

type DeleteUser = "user" :> Verb 'DELETE 204 '[JSON] NoContent

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
        <$> requiredField "uuid" "backup identifier"
          .= backupInfoUUID
        <*> requiredField "time" "backup timestamp"
          .= backupInfoTime
        <*> requiredField "size" "total size, in bytes"
          .= backupInfoSize

type BackupUUID = UUID BackupInfo

type GetListBackups = "backups" :> Get '[JSON] [BackupInfo]

type PostBackup = "backup" :> Post '[JSON] BackupUUID

type GetBackup = "backup" :> Capture "backup" BackupUUID :> StreamGet NoFraming OctetStream (SourceIO ByteString)

type PutRestoreBackup = "backup" :> Capture "backup" BackupUUID :> "restore" :> Verb 'PUT 204 '[JSON] NoContent

type DeleteBackup = "backup" :> Capture "backup" BackupUUID :> Verb 'DELETE 204 '[JSON] NoContent

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
        <$> requiredFieldWith "items" (codecViaAeson "Mergeful.SyncRequest") "sync request of the smos files"
          .= syncRequestItems

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
        <$> requiredField "server-id" "identifier of the server"
          .= syncResponseServerId
        <*> requiredFieldWith "items" (codecViaAeson "Mergeful.SyncResponse") "sync response of the smos files"
          .= syncResponseItems

instance FromHttpApiData (Path Rel File) where
  parseQueryParam t = left (T.pack . displayException :: SomeException -> Text) $ parseRelFile (T.unpack t)

instance ToHttpApiData (Path Rel File) where
  toQueryParam = T.pack . fromRelFile

type GetListSmosFiles = "files" :> Get '[JSON] (DirForest SmosFile)

type GetSmosFile = "file" :> QueryParam' '[Required, Strict] "path" (Path Rel File) :> Get '[JSON] SmosFile

type PutSmosFile = "file" :> QueryParam' '[Required, Strict] "path" (Path Rel File) :> ReqBody '[JSON] SmosFile :> Verb 'PUT 204 '[JSON] NoContent

data BookingSettings = BookingSettings
  { bookingSettingName :: !Text,
    bookingSettingEmailAddress :: !Text,
    bookingSettingTimeZone :: !TZLabel,
    bookingSettingAllowedDays :: !(Set DayOfWeek)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON, ToYaml) via (Autodocodec BookingSettings)

instance Validity BookingSettings

instance NFData BookingSettings

instance HasCodec BookingSettings where
  codec =
    object "BookingSettings" $
      BookingSettings
        <$> requiredField "name" "user display name"
          .= bookingSettingName
        <*> requiredField "email-address" "user email address"
          .= bookingSettingEmailAddress
        <*> requiredField "time-zone" "user time zone"
          .= bookingSettingTimeZone
        <*> optionalFieldWithDefaultWith
          "allowed-days"
          (dimapCodec S.fromList S.toList (listCodec shownBoundedEnumCodec))
          (S.fromList [Monday, Tuesday, Wednesday, Thursday, Friday])
          "allowed days"
          .= bookingSettingAllowedDays

type GetBookingSettings = "book" :> Capture "username" Username :> "settings" :> Get '[JSON] BookingSettings

type GetBookingSlots = "book" :> Capture "username" Username :> "slots" :> Get '[JSON] BookingSlots

data BookingSlots = BookingSlots {bookingSlots :: Map LocalTime NominalDiffTime}
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec BookingSlots)

instance Validity BookingSlots

instance NFData BookingSlots

instance HasCodec BookingSlots where
  codec =
    object "BookingSlots" $
      BookingSlots
        <$> requiredField "slots" "slots with their duration"
          .= bookingSlots

type PostBooking =
  "book"
    :> Capture "username" Username
    :> ReqBody '[JSON] Booking
    :> Post '[ICal] ICal.ICalendar

data Booking = Booking
  { bookingClientName :: !Text,
    bookingClientEmailAddress :: !Text,
    bookingClientTimeZone :: !TZLabel,
    bookingUTCTime :: !UTCTime,
    bookingDuration :: !NominalDiffTime,
    bookingExtraInfo :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Booking)

instance Validity Booking

instance NFData Booking

instance HasCodec Booking where
  codec =
    object "Booking" $
      Booking
        <$> requiredField "name" "client name"
          .= bookingClientName
        <*> requiredField "email-address" "client email address"
          .= bookingClientEmailAddress
        <*> requiredField "time-zone" "client time zone"
          .= bookingClientTimeZone
        <*> requiredField "time" "local time"
          .= bookingUTCTime
        <*> requiredField "duration" "duration"
          .= bookingDuration
        <*> optionalField "extra-info" "extra information for both participants"
          .= bookingExtraInfo

data ICal

instance Accept ICal where
  contentType Proxy = "text/calendar"

instance MimeRender ICal [ICal.Calendar] where
  mimeRender Proxy = LB.fromStrict . ICal.renderICalendarByteString

instance MimeUnrender ICal [ICal.Calendar] where
  mimeUnrender Proxy = left displayException . fmap fst . ICal.runConform . ICal.parseICalendarByteString . LB.toStrict

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

type PostMigrateFiles = "migrator-files" :> Verb 'POST 204 '[JSON] NoContent

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
        <$> requiredField "name" "user name"
          .= userInfoUsername
        <*> optionalFieldWithDefault "admin" False "whether the user is an admin"
          .= userInfoAdmin
        <*> requiredField "created" "user creation time"
          .= userInfoCreated
        <*> requiredField "last-login" "last time the user logged in"
          .= userInfoLastLogin
        <*> requiredField "last-use" "last time the user used the API"
          .= userInfoLastUse
        <*> requiredField "subscribed" "user subscription status"
          .= userInfoSubscribed

type PutUserSubscription = "users" :> Capture "username" Username :> ReqBody '[JSON] UTCTime :> Verb 'PUT 204 '[JSON] NoContent

bookingFilePath :: Path Rel File
bookingFilePath = [relfile|server-config/booking.yaml|]
