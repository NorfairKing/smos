{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.GitHub.Issue.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import GitHub
import Smos.GitHub.Issue

instance GenValid GitHubUrl

instance GenValid IssueNumber

instance GenValid (Name a)
