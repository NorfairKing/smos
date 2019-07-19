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
  ads <- lift $ asks $ smosReportConfigArchiveFileSpec . smosQueryConfigReportConfig
  ha <- lift $ asks smosQueryConfigHideArchive
  case ads of
    ArchiveInWorkflow rf -> do
      let source =
            (case ha of
               HideArchive -> sourceFilesInNonHiddenDirsRecursivelyExceptSubdir rf wd
               Don'tHideArchive -> sourceFilesInNonHiddenDirsRecursively wd)
      source .| filterSmosFiles
    _ -> do
      ad <- lift askArchiveDir
      let maybeFilterOutArchived =
            (case ha of
               HideArchive -> (filterOutDir ad .|)
               Don'tHideArchive -> id)
      sourceFilesInNonHiddenDirsRecursively wd .| maybeFilterOutArchived filterSmosFiles

-- TODO I think we can do fancier filtering based on the other ArchiveDirSpecs
filterOutDir :: Monad m => Path Abs Dir -> ConduitT RootedPath RootedPath m ()
filterOutDir ad = Conduit.filter (\rp -> not $ isProperPrefixOf ad $ resolveRootedPath rp)
