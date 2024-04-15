{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.TestUtils where

import Control.DeepSeq
import qualified Data.Set as S
import Data.Word
import Database.Persist.Sqlite as DB
import qualified Network.HTTP.Client as Http
import Servant.Auth.Client as Auth
import Servant.Auth.Server as Auth
import Smos.API.Gen ()
import Smos.Client
import Smos.Server.Handler.Import as Server
import Smos.Server.Looper
import Smos.Server.Serve as Server
import Test.QuickCheck
import Test.Syd
import Test.Syd.Persistent.Sqlite
import Test.Syd.Validity
import Test.Syd.Wai
import UnliftIO

data ServerTestEnv = ServerTestEnv
  { serverTestEnvClientEnv :: !ClientEnv,
    serverTestEnvServerEnv :: !ServerEnv
  }

runServerTestEnvM :: ServerTestEnv -> ServerTestEnvM a -> IO a
runServerTestEnvM = flip runReaderT

type ServerTestEnvM a = ReaderT ServerTestEnv IO a

serverEnvSpec :: TestDef '[Http.Manager] ServerTestEnv -> Spec
serverEnvSpec = modifyMaxSuccess (`div` 20) . managerSpec . setupAroundWith' (\man () -> serverTestEnvSetupFunc man)

serverTestEnvSetupFunc :: Http.Manager -> SetupFunc ServerTestEnv
serverTestEnvSetupFunc man = do
  pool <- serverConnectionPoolSetupFunc
  senv <- serverEnvSetupFunc pool
  cenv <- clientEnvSetupFunc man senv
  pure $
    ServerTestEnv
      { serverTestEnvServerEnv = senv,
        serverTestEnvClientEnv = cenv
      }

serverEnvDB :: SqlPersistT IO a -> ServerTestEnvM a
serverEnvDB func = do
  pool <- asks $ serverEnvConnection . serverTestEnvServerEnv
  liftIO $ DB.runSqlPool func pool

serverEnvLooper :: Looper a -> ServerTestEnvM a
serverEnvLooper func = do
  ServerEnv {..} <- asks serverTestEnvServerEnv
  let env =
        LooperEnv
          { looperEnvConnection = serverEnvConnection,
            looperEnvCompressionLevel = serverEnvCompressionLevel,
            looperEnvMaxBackupsPerPeriodPerUser = defaultPeriods
          }
  liftIO $ runLoggingT (runReaderT func env) serverEnvLogFunc

serverEnvClientOrErr :: (NFData a) => ClientM a -> ServerTestEnvM a
serverEnvClientOrErr func = do
  cenv <- asks serverTestEnvClientEnv
  liftIO $ testClient cenv func

withServerEnvNewUser :: (Token -> ServerTestEnvM ()) -> ServerTestEnvM ()
withServerEnvNewUser func = do
  cenv <- asks serverTestEnvClientEnv
  withNewUser cenv func

serverConnectionPoolSetupFunc :: SetupFunc ConnectionPool
serverConnectionPoolSetupFunc = connectionPoolSetupFunc serverAutoMigration

type ServerSpec = TestDef '[Http.Manager] ClientEnv

serverSpec :: ServerSpec -> Spec
serverSpec = modifyMaxSuccess (`div` 20) . managerSpec . setupAroundWith' (\man () -> serverSetupFunc man)

serverSetupFunc :: Http.Manager -> SetupFunc ClientEnv
serverSetupFunc man = serverConnectionPoolSetupFunc >>= serverSetupFunc' man

serverSetupFunc' :: Http.Manager -> ConnectionPool -> SetupFunc ClientEnv
serverSetupFunc' man pool = serverEnvSetupFunc pool >>= clientEnvSetupFunc man

clientEnvSetupFunc :: Http.Manager -> ServerEnv -> SetupFunc ClientEnv
clientEnvSetupFunc man env = do
  let application = Server.makeSyncApp env
  p <- applicationSetupFunc application
  -- The fromIntegral is safe because it's PortNumber -> Int
  pure $ mkClientEnv man (BaseUrl Http "127.0.0.1" (fromIntegral p) "")

serverEnvSetupFunc :: ConnectionPool -> SetupFunc ServerEnv
serverEnvSetupFunc pool = liftIO $ do
  uuid <- nextRandomUUID
  jwtKey <- Auth.generateKey
  -- We turn logging off while tests pass, but for debugging the logs will
  -- probably be useful.  So while debugging you may want to uncomment the
  -- next line and comment out the one after that.
  --
  -- logFunc <- runStderrLoggingT askLoggerIO
  let logFunc = evaluatingLog -- Evaluate the log lines, but dont print them.
  priceVar <- newEmptyMVar
  pure $
    ServerEnv
      { serverEnvServerUUID = uuid,
        serverEnvConnection = pool,
        serverEnvCookieSettings = defaultCookieSettings,
        serverEnvJWTSettings = defaultJWTSettings jwtKey,
        serverEnvPasswordDifficulty = 4, -- The lowest (fastest)
        serverEnvCompressionLevel = 1, -- The lowest (fastest)
        serverEnvLogFunc = logFunc,
        serverEnvMaxBackupSizePerUser = Nothing,
        serverEnvAdmin = Just testAdminUsername,
        serverEnvBookingEmailAddress = Nothing,
        serverEnvPriceCache = priceVar,
        serverEnvMonetisationSettings = Nothing
      }

evaluatingLog :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
evaluatingLog loc source level str = do
  _ <- evaluate loc
  _ <- evaluate source
  _ <- evaluate level
  _ <- evaluate str
  pure ()

testAdminUsername :: Username
testAdminUsername = "admin"

testAdminPassword :: Text
testAdminPassword = "dummy"

testAdminRegister :: Register
testAdminRegister = Register {registerUsername = testAdminUsername, registerPassword = testAdminPassword}

registerLogin :: Register -> Login
registerLogin register =
  Login {loginUsername = registerUsername register, loginPassword = registerPassword register}

testLogin :: ClientEnv -> Login -> IO Token
testLogin cenv lf = do
  errOrRes <- login cenv lf
  case errOrRes of
    Left err -> expectationFailure $ "Failed to login: " <> show err
    Right t -> pure t

withAdminUser :: (MonadIO m) => ClientEnv -> (Token -> m ()) -> m ()
withAdminUser cenv func =
  withNewGivenUser cenv testAdminRegister func

withNewUser :: (MonadUnliftIO m) => ClientEnv -> (Token -> m ()) -> m ()
withNewUser cenv func = withNewUserAndData cenv $ const func

withNewUserAndData :: (MonadUnliftIO m) => ClientEnv -> (Register -> Token -> m a) -> m a
withNewUserAndData cenv func = do
  r <- liftIO randomRegistration
  withNewGivenUser cenv r $ func r

withNewGivenUser :: (MonadIO m) => ClientEnv -> Register -> (Token -> m a) -> m a
withNewGivenUser cenv r func = do
  t <-
    liftIO $ do
      NoContent <- testClient cenv $ clientPostRegister r
      testLogin cenv (registerLogin r)
  func t

withNewRegisteredUser :: (MonadIO m) => ClientEnv -> (Register -> m a) -> m a
withNewRegisteredUser cenv func = do
  r <- liftIO randomRegistration
  NoContent <- liftIO $ testClient cenv $ clientPostRegister r
  func r

randomRegistration :: IO Register
randomRegistration = do
  u1 <- nextRandomUUID :: IO (UUID Username) -- Dummy's that are significantly likely to be random enough
  u2 <- nextRandomUUID :: IO (UUID Password)
  un <- parseUsername $ uuidText u1
  let pw = uuidText u2
  pure Register {registerUsername = un, registerPassword = pw}

testClient :: (NFData a) => ClientEnv -> ClientM a -> IO a
testClient cenv func = do
  errOrRes <- runClient cenv func
  case errOrRes of
    Left err -> expectationFailure $ show err
    Right r -> pure r

forAllBookingDuration :: (Testable t) => (BookingSettings -> Word8 -> t) -> Property
forAllBookingDuration func =
  forAllValid $ \bookingSettingsPrototype ->
    forAllValid $ \minutes ->
      let bookingSettings =
            bookingSettingsPrototype
              { bookingSettingAllowedDurations =
                  S.insert
                    minutes
                    (bookingSettingAllowedDurations bookingSettingsPrototype)
              }
       in func bookingSettings minutes
