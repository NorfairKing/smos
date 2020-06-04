{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetNextActionReport
  ( serveGetNextActionReport,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.Time
import Path
import Smos.Data
import Smos.Report.Next
import Smos.Report.Path
import Smos.Server.Handler.Import

serveGetNextActionReport :: AuthCookie -> SyncHandler NextActionReport
serveGetNextActionReport (AuthCookie un) = do
  mu <- runDB $ getBy $ UniqueUsername un
  case mu of
    Nothing -> throwError err404
    Just (Entity uid _) -> do
      liftIO $ getCurrentTime >>= print
      acqSource <- runDB $ selectSourceRes [ServerFileUser ==. uid] []
      report <- liftIO $ withAcquire acqSource $ \source ->
        runConduit $ source .| parseServerFileC .| nextActionReportConduit
      liftIO $ getCurrentTime >>= print
      pure report

parseServerFileC :: Monad m => ConduitT (Entity ServerFile) (RootedPath, SmosFile) m ()
parseServerFileC = C.concatMap $ \(Entity _ ServerFile {..}) ->
  if isProperPrefixOf [reldir|archive|] serverFilePath
    then Nothing
    else
      if fileExtension serverFilePath == ".smos"
        then case parseSmosFile serverFileContents of
          Left _ -> Nothing
          Right sf -> Just (Relative [absdir|/|] serverFilePath, sf)
        else Nothing
