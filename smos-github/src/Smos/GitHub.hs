{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.GitHub
  ( smosGitHub,
  )
where

import Conduit
import Control.Concurrent.Async
import Control.Monad
import Data.Aeson
import qualified Data.Conduit.Combinators as C
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import GHC.Generics (Generic)
import GitHub (IssueNumber (..), Owner, Repo, github, unIssueNumber)
import qualified GitHub
import GitHub.Auth (Auth (OAuth))
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
  trips <-
    runConduit $
      streamSmosFilesFromWorkflowRel HideArchive setDirectorySettings
        .| parseSmosFilesRel wd
        .| printShouldPrint (PrintWarning stderr)
        .| smosFileEntries
        .| C.concatMap (\(rf, e) -> (,,) rf e <$> parseEntryGitHubUrl e) -- C.concatMap generalises mapMaybe
        .| sinkList
  let mAuth = OAuth . TE.encodeUtf8 <$> setGithubOauthToken
  report <- completeListReport mAuth trips
  putChunks $ renderGitHubListReport setColourConfig report

parseEntryGitHubUrl :: Entry -> Maybe GitHubUrl
parseEntryGitHubUrl e = do
  guard (not (entryIsDone e))
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

newtype GitHubListReport = GitHubListReport {githubListReportRows :: [ListReportRow]}
  deriving (Show, Eq, Generic)

completeListReport :: Maybe GitHub.Auth -> [(Path Rel File, Entry, GitHubUrl)] -> IO GitHubListReport
completeListReport mAuth = fmap (GitHubListReport . sortOn listReportRowBall) . mapConcurrently (fillInRow mAuth)

renderGitHubListReport :: ColourConfig -> GitHubListReport -> [Chunk]
renderGitHubListReport cc GitHubListReport {..} =
  formatAsBicolourTable cc $
    headerRow : map renderListReportRow githubListReportRows

headerRow :: [Chunk]
headerRow =
  map underline ["file", "state", "header", "owner", "repo", "number", "state", "update"]

data ListReportRow = ListReportRow
  { listReportRowPath :: !(Path Rel File),
    listReportRowEntry :: !Entry,
    listReportRowGitHubUrl :: !GitHubUrl,
    listReportRowState :: !(Maybe GitHub.IssueState),
    listReportRowMerged :: !(Maybe UTCTime),
    listReportRowBall :: !(Maybe Ball)
  }
  deriving (Show, Eq, Generic)

-- Note: The order of the constructors matters for the ordering of the list
data Ball = BallInOurCourt | BallInTheirCourt
  deriving (Show, Eq, Ord, Generic)

fillInRow :: Maybe GitHub.Auth -> (Path Rel File, Entry, GitHubUrl) -> IO ListReportRow
fillInRow mAuth (listReportRowPath, listReportRowEntry, listReportRowGitHubUrl) = do
  let mGithub :: FromJSON result => GitHub.Request rw result -> IO (Maybe result)
      mGithub req = case mAuth of
        Nothing -> pure Nothing
        Just auth -> do
          errOrRes <- github auth req
          pure $ case errOrRes of
            Left _ -> Nothing -- TODO this is debatable, we probably want to die instead.
            Right r -> Just r
  (listReportRowState, listReportRowMerged, mRemoteLastUpdate) <- case listReportRowGitHubUrl of
    PullRequestUrl o r i -> do
      pr <- mGithub $ GitHub.pullRequestR o r i
      pure
        ( GitHub.pullRequestState <$> pr,
          pr >>= GitHub.pullRequestMergedAt,
          GitHub.pullRequestUpdatedAt <$> pr
        )
    IssueUrl o r i -> do
      iss <- mGithub $ GitHub.issueR o r i
      pure
        ( GitHub.issueState <$> iss,
          Nothing,
          GitHub.issueUpdatedAt <$> iss
        )
  let listReportRowBall = do
        elu <- entryLastUpdate listReportRowEntry
        rlu <- mRemoteLastUpdate
        let dependingOnTime = if rlu >= elu then BallInOurCourt else BallInTheirCourt
        pure $ case entryState listReportRowEntry of
          Just "WAITING" -> dependingOnTime
          _ -> BallInOurCourt
  pure ListReportRow {..}

entryLastUpdate :: Entry -> Maybe UTCTime
entryLastUpdate = fmap stateHistoryEntryTimestamp . listToMaybe . unStateHistory . entryStateHistory

renderListReportRow :: ListReportRow -> [Chunk]
renderListReportRow ListReportRow {..} =
  concat
    [ [ pathChunk listReportRowPath,
        mTodoStateChunk $ entryState listReportRowEntry,
        headerChunk $ entryHeader listReportRowEntry
      ],
      githubUrlChunks listReportRowGitHubUrl,
      [ case listReportRowMerged of
          Nothing -> maybe "" stateChunk listReportRowState
          Just _ -> "merged",
        maybe "" ballChunk listReportRowBall
      ]
    ]

githubUrlChunks :: GitHubUrl -> [Chunk]
githubUrlChunks = \case
  PullRequestUrl n r i -> [nameChunk n, nameChunk r, issueNumberChunk i]
  IssueUrl n r i -> [nameChunk n, nameChunk r, issueNumberChunk i]

nameChunk :: Name a -> Chunk
nameChunk = chunk . untagName

issueNumberChunk :: IssueNumber -> Chunk
issueNumberChunk = chunk . T.pack . show . unIssueNumber

stateChunk :: GitHub.IssueState -> Chunk
stateChunk =
  chunk . \case
    GitHub.StateOpen -> "open"
    GitHub.StateClosed -> "closed"

ballChunk :: Ball -> Chunk
ballChunk = \case
  BallInOurCourt -> fore yellow "ready"
  BallInTheirCourt -> fore blue "waiting"
