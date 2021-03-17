{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.GitHub
  ( smosGitHub,
  )
where

import Conduit
import Control.Monad
import qualified Data.Conduit.Combinators as C
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Generics (Generic)
import GitHub (IssueNumber (..), Owner, Repo, unIssueNumber)
import GitHub.Data.Name
import Network.URI
import Path
import Smos.Data
import Smos.GitHub.OptParse
import Smos.Query.Config (ColourConfig)
import Smos.Query.Formatting
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.ShouldPrint
import Smos.Report.Streaming
import System.IO
import Text.Read

smosGitHub :: IO ()
smosGitHub = do
  Instructions _ Settings {..} <- getInstructions
  wd <- resolveDirWorkflowDir setDirectorySettings
  case setGithubOauthToken of
    Nothing -> putStrLn "WARNING: No OAUTH token configured. Some functionality may not work without it."
    Just _ -> pure ()
  report <-
    runConduit $
      streamSmosFilesFromWorkflowRel HideArchive setDirectorySettings
        .| parseSmosFilesRel wd
        .| printShouldPrint (PrintWarning stderr)
        .| smosFileEntries
        .| C.concatMap (\(rf, e) -> (,,) rf e <$> parseEntryGitHubUrl e) -- C.concatMap generalises mapMaybe
        .| (GitHubListReport <$> sinkList)
  putChunks $ renderGitHubListReport setColourConfig report

newtype GitHubListReport = GitHubListReport
  { githubListReportRows :: [(Path Rel File, Entry, GitHubUrl)]
  }

renderGitHubListReport :: ColourConfig -> GitHubListReport -> [Chunk]
renderGitHubListReport cc GitHubListReport {..} =
  formatAsBicolourTable cc $
    map underline ["file", "header", "owner", "repo", "issue"] :
    map
      ( \(rf, e, gu) ->
          [pathChunk rf, headerChunk $ entryHeader e] ++ githubUrlChunks gu
      )
      githubListReportRows

parseEntryGitHubUrl :: Entry -> Maybe GitHubUrl
parseEntryGitHubUrl e = do
  up <- M.lookup "url" $ entryProperties e
  uri <- parseURI (T.unpack (propertyValueText up))
  ua <- uriAuthority uri
  guard (uriRegName ua == "github.com")
  let segments = pathSegments uri
  -- Url is of this form
  -- https://github.com/owner/repo/pull/167
  -- https://github.com/owner/repo/issues/167
  let mkN = N . T.pack
  case segments of
    (owner : repo : "pull" : num : _) -> PullRequestUrl (mkN owner) (mkN repo) <$> (IssueNumber <$> readMaybe num)
    (owner : repo : "issue" : num : _) -> IssueUrl (mkN owner) (mkN repo) <$> (IssueNumber <$> readMaybe num)
    _ -> Nothing

data GitHubUrl
  = PullRequestUrl (Name Owner) (Name Repo) IssueNumber
  | IssueUrl (Name Owner) (Name Repo) IssueNumber
  deriving (Show, Eq, Generic)

githubUrlChunks :: GitHubUrl -> [Chunk]
githubUrlChunks = \case
  PullRequestUrl n r i -> [nameChunk n, nameChunk r, issueNumberChunk i]
  IssueUrl n r i -> [nameChunk n, nameChunk r, issueNumberChunk i]

nameChunk :: Name a -> Chunk
nameChunk = chunk . untagName

issueNumberChunk :: IssueNumber -> Chunk
issueNumberChunk = chunk . T.pack . show . unIssueNumber
