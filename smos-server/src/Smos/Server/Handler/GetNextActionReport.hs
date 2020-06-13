{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetNextActionReport
  ( serveGetNextActionReport,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import Path
import Smos.Data
import Smos.Report.Next
import Smos.Server.Handler.Import

serveGetNextActionReport :: AuthCookie -> SyncHandler NextActionReport
serveGetNextActionReport (AuthCookie un) = withUserId un $ \uid -> do
  acqSource <- runDB $ selectSourceRes [ServerFileUser ==. uid] []
  liftIO $ withAcquire acqSource $ \source ->
    runConduit $ source .| parseServerFileC .| nextActionReportConduit Nothing

-- TODO deal with the archive using 'hideArchive'.
parseServerFileC :: Monad m => ConduitT (Entity ServerFile) (Path Rel File, SmosFile) m ()
parseServerFileC = C.concatMap $ \(Entity _ ServerFile {..}) ->
  if isProperPrefixOf [reldir|archive|] serverFilePath
    then Nothing
    else
      if fileExtension serverFilePath == ".smos"
        then case parseSmosFile serverFileContents of
          Left _ -> Nothing
          Right sf -> Just (serverFilePath, sf)
        else Nothing
