{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.GitHub.Command.List
  ( githubList,
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
import GitHub (IssueNumber (..), github, unIssueNumber)
import qualified GitHub
import GitHub.Auth (Auth (OAuth))
import GitHub.Data.Name
import Path
import Smos.CLI.Colour
import Smos.CLI.Formatting
import Smos.Data
import Smos.GitHub.Issue
import Smos.GitHub.OptParse
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.ShouldPrint
import Smos.Report.Streaming
import System.IO
import Text.Colour
import Text.Colour.Term

githubList :: Settings -> IO ()
githubList Settings {..} = do
  wd <- resolveDirWorkflowDir setDirectorySettings
  case setGitHubOauthToken of
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
  let mAuth = OAuth . TE.encodeUtf8 <$> setGitHubOauthToken
  report <- completeListReport mAuth trips
  putChunksLocale $ renderGitHubListReport setColourConfig report

parseEntryGitHubUrl :: Entry -> Maybe GitHubUrl
parseEntryGitHubUrl e = do
  guard (not (entryIsDone e))
  up <- M.lookup "url" $ entryProperties e
  parseGitHubUrl (T.unpack (propertyValueText up))

newtype GitHubListReport = GitHubListReport {githubListReportRows :: [ListReportRow]}
  deriving (Show, Eq, Generic)

completeListReport :: Maybe GitHub.Auth -> [(Path Rel File, Entry, GitHubUrl)] -> IO GitHubListReport
completeListReport mAuth = fmap (GitHubListReport . sortOn listReportRowBall) . mapConcurrently (fillInRow mAuth)

renderGitHubListReport :: ColourSettings -> GitHubListReport -> [Chunk]
renderGitHubListReport colourSettings GitHubListReport {..} =
  formatAsBicolourTable colourSettings $
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
  let mGitHub :: FromJSON result => GitHub.Request rw result -> IO (Maybe result)
      mGitHub req = case mAuth of
        Nothing -> pure Nothing
        Just auth -> do
          errOrRes <- github auth req
          pure $ case errOrRes of
            Left _ -> Nothing -- TODO this is debatable, we probably want to die instead.
            Right r -> Just r
  (listReportRowState, listReportRowMerged, mRemoteLastUpdate) <- case listReportRowGitHubUrl of
    PullRequestUrl o r i -> do
      pr <- mGitHub $ GitHub.pullRequestR o r i
      pure
        ( GitHub.pullRequestState <$> pr,
          pr >>= GitHub.pullRequestMergedAt,
          GitHub.pullRequestUpdatedAt <$> pr
        )
    IssueUrl o r i -> do
      iss <- mGitHub $ GitHub.issueR o r i
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
