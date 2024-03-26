module Smos.Server.Handler.GetListSmosFiles
  ( serveGetListSmosFiles,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.DirForest (DirForest)
import qualified Data.DirForest as DF
import Path
import Smos.Data
import Smos.Server.Handler.Import

serveGetListSmosFiles :: AuthNCookie -> ServerHandler (DirForest SmosFile)
serveGetListSmosFiles ac = withUserId ac $ \uid -> do
  let go :: DirForest SmosFile -> (Path Rel File, SmosFile) -> DirForest SmosFile
      go df (rf, sf) = case DF.insertFile rf sf df of
        Left _ -> df
        Right df' -> df'
      c :: ConduitT (Path Rel File, SmosFile) Void ServerHandler (DirForest SmosFile)
      c = C.foldl go DF.empty
  streamSmosFiles uid Don'tHideArchive c
