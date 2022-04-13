module Smos.GitHub
  ( smosGitHub,
  )
where

import Smos.GitHub.Command
import Smos.GitHub.OptParse

smosGitHub :: IO ()
smosGitHub = do
  Instructions d sets <- getInstructions
  case d of
    DispatchList -> githubList sets
    DispatchImport importSets -> githubImport sets importSets
