module Smos.GitHub
  ( smosGitHub,
  )
where

import Smos.GitHub.OptParse

smosGitHub :: IO ()
smosGitHub = do
  getInstructions >>= print
