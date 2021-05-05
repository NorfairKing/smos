{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.Import
  ( module X,
    withUserId,
    streamSmosFiles,
    asAdmin,
  )
where

import Conduit
import Control.Monad.Except as X
import Control.Monad.Logger as X
import Control.Monad.Reader as X
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import Data.Time as X
import Data.UUID.Typed as X
import Path as X
import Path.IO as X hiding (getPermissions)
import Servant.API as X
import Servant.Server as X
import Smos.API as X
import Smos.Data hiding (parseHeader)
import Smos.Report.Archive as X
import Smos.Server.Constants as X
import Smos.Server.DB as X
import Smos.Server.Env as X
import Text.Show.Pretty as X

withUserId :: AuthNCookie -> (UserId -> ServerHandler a) -> ServerHandler a
withUserId AuthNCookie {..} func = do
  mu <- runDB $ getBy $ UniqueUsername authNCookieUsername
  case mu of
    Nothing -> throwError err404
    Just (Entity uid _) -> do
      logInfoN $ T.unwords ["Succesfully authenticated user", T.pack (show (usernameText authNCookieUsername))]
      func uid

streamSmosFiles :: UserId -> HideArchive -> ConduitT (Path Rel File, SmosFile) Void IO r -> ServerHandler r
streamSmosFiles uid ha conduit = do
  acqSource <- runDB $ selectSourceRes [ServerFileUser ==. uid] []
  liftIO $
    withAcquire acqSource $ \source ->
      runConduit $ source .| parseServerFileC ha .| conduit

parseServerFileC :: Monad m => HideArchive -> ConduitT (Entity ServerFile) (Path Rel File, SmosFile) m ()
parseServerFileC ha = C.concatMap $ \(Entity _ ServerFile {..}) ->
  let filePred = case ha of
        HideArchive -> not . isProperPrefixOf [reldir|archive|]
        Don'tHideArchive -> const True
   in if filePred serverFilePath && fileExtension serverFilePath == Just ".smos"
        then case parseSmosFile serverFileContents of
          Left _ -> Nothing
          Right sf -> Just (serverFilePath, sf)
        else Nothing

asAdmin :: Username -> ServerHandler a -> ServerHandler a
asAdmin username func = do
  mAdmin <- asks serverEnvAdmin
  let isAdmin = mAdmin == Just username
  if isAdmin then func else throwError err404
