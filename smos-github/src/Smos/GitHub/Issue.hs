{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.GitHub.Issue where

import Control.Monad
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)
import GitHub (IssueNumber (..), Owner, Repo)
import GitHub.Data.Name
import Network.URI
import Text.Read

parseGitHubUrl :: String -> Maybe GitHubUrl
parseGitHubUrl url = do
  uri <- parseURI url
  ua <- uriAuthority uri
  guard (uriRegName ua == "github.com")
  let segments = pathSegments uri
  let mkN = N . T.pack
  case segments of
    (owner : repo : "pull" : num : _) -> PullRequestUrl (mkN owner) (mkN repo) <$> (IssueNumber <$> readMaybe num)
    (owner : repo : "issues" : num : _) -> IssueUrl (mkN owner) (mkN repo) <$> (IssueNumber <$> readMaybe num)
    _ -> Nothing

data GitHubUrl
  = PullRequestUrl (Name Owner) (Name Repo) IssueNumber
  | IssueUrl (Name Owner) (Name Repo) IssueNumber
  deriving (Show, Eq, Generic)

instance Validity GitHubUrl

instance Validity IssueNumber

instance Validity (Name a)
