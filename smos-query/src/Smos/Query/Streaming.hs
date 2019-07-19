module Smos.Query.Streaming where

import Conduit
import Data.Conduit.Combinators as Conduit
import Path

import Smos.Report.Path
import Smos.Report.Streaming

import Smos.Query.Config

streamSmosFiles :: ConduitT i RootedPath Q ()
streamSmosFiles = do
  wd <- lift askWorkflowDir
  ad <- lift askArchiveDir
  ha <- lift $ asks smosQueryConfigHideArchive
  let maybeFilterOutArchived =
        (case ha of
           HideArchive -> (filterOutDir ad .|)
           Don'tHideArchive -> id)
  sourceFilesInNonHiddenDirsRecursively wd .| maybeFilterOutArchived filterSmosFiles

-- TODO we can do fancier filtering in the default case
filterOutDir :: Monad m => Path Abs Dir -> ConduitT RootedPath RootedPath m ()
filterOutDir ad = Conduit.filter (\rp -> not $ isProperPrefixOf ad $ resolveRootedPath rp)
