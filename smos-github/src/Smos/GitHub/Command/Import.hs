{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.GitHub.Command.Import where

import Control.Monad
import Data.Aeson
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)
import GitHub (github)
import qualified GitHub
import Path
import Path.IO
import Smos.Data
import Smos.GitHub.Issue
import Smos.GitHub.OptParse
import Smos.Report.Config
import System.Exit

githubImport :: Settings -> ImportSettings -> IO ()
githubImport Settings {..} ImportSettings {..} = do
  case setGithubOauthToken of
    Nothing -> putStrLn "WARNING: No OAUTH token configured. Some functionality may not work without it."
    Just _ -> pure ()
  case parseGitHubUrl importSetUrl of
    Nothing -> die $ "Could not parse as a URL of a github issue or pull request:" <> importSetUrl
    Just gitHubUrl -> do
      projectsDir <- resolveDirProjectsDir setDirectorySettings
      let mAuth = GitHub.OAuth . TE.encodeUtf8 <$> setGithubOauthToken
      mDetails <- forM mAuth $ \auth ->
        fetchDetails auth gitHubUrl
      now <- getCurrentTime
      case (,)
        <$> renderProjectPath gitHubUrl
        <*> renderSmosProject now importSetUrl gitHubUrl mDetails of
        Nothing -> die "Failed to import this issue/pr." -- TODO could we give a better error?
        Just (projectPath, smosFile) -> do
          let path = projectsDir </> projectPath
          exists <- doesFileExist path
          when (exists && not importSetForce) $ die $ "File already exists, not overwriting: " <> show path
          putStrLn $ unwords ["Importing to", fromAbsFile path]
          writeSmosFile path smosFile

renderProjectPath :: GitHubUrl -> Maybe (Path Rel File)
renderProjectPath gitHubUrl = do
  let filePath = case gitHubUrl of
        IssueUrl _ repoName issueNumber ->
          mconcat
            [ T.unpack (GitHub.untagName repoName),
              "/issue-",
              show $ GitHub.unIssueNumber issueNumber,
              ".smos"
            ]
        PullRequestUrl _ repoName pullRequestNumber ->
          mconcat
            [ T.unpack (GitHub.untagName repoName),
              "/pr-",
              show $ GitHub.unIssueNumber pullRequestNumber,
              ".smos"
            ]
  parseRelFile filePath

renderSmosProject :: UTCTime -> String -> GitHubUrl -> Maybe ImportDetails -> Maybe SmosFile
renderSmosProject now urlString gitHubUrl mDetails = do
  let ownerNameText = GitHub.untagName $ case gitHubUrl of
        IssueUrl ownerName _ _ -> ownerName
        PullRequestUrl ownerName _ _ -> ownerName
  let repoNameText = GitHub.untagName $ case gitHubUrl of
        IssueUrl _ repoName _ -> repoName
        PullRequestUrl _ repoName _ -> repoName
  let titleText = case gitHubUrl of
        IssueUrl _ _ issueNumber ->
          let issueNumberText = T.pack $ show $ GitHub.unIssueNumber issueNumber
           in mconcat
                [ repoNameText,
                  " issue ",
                  issueNumberText
                ]
        PullRequestUrl _ _ pullRequestNumber ->
          let pullRequestNumberText = T.pack $ show $ GitHub.unIssueNumber pullRequestNumber
           in mconcat
                [ repoNameText,
                  " pull request ",
                  pullRequestNumberText
                ]
  title <- header titleText
  urlProperty <- propertyValue $ T.pack urlString
  goalProperty <-
    propertyValue $
      T.unwords
        [ "Resolve",
          titleText
        ]
  ownerProperty <- propertyValue ownerNameText
  repoProperty <- propertyValue repoNameText
  detailProperties <- case mDetails of
    Nothing -> pure M.empty
    Just ImportDetails {..} -> do
      titleProperty <- propertyValue importDetailTitle
      pure $ M.singleton "title" titleProperty
  let properties =
        mconcat
          [ M.fromList
              [ ("url", urlProperty),
                ("goal", goalProperty),
                ("owner", ownerProperty),
                ("repo", repoProperty)
              ],
            detailProperties
          ]
  titleEntry <-
    entrySetState
      now
      (Just "TODO")
      ((newEntry title) {entryProperties = properties})
  pure $
    SmosFile
      [ Node
          titleEntry
          []
      ]

data ImportDetails = ImportDetails
  { importDetailTitle :: Text
  }
  deriving (Show, Eq, Generic)

instance Validity ImportDetails

fetchDetails :: GitHub.Auth -> GitHubUrl -> IO ImportDetails
fetchDetails auth gitHubUrl = do
  let errOrGitHub :: FromJSON result => GitHub.Request rw result -> IO result
      errOrGitHub request = do
        errOrResult <- github auth request
        case errOrResult of
          Left err -> die $ show err
          Right result -> pure result
  case gitHubUrl of
    PullRequestUrl o r i -> do
      GitHub.PullRequest {..} <- errOrGitHub $ GitHub.pullRequestR o r i
      let importDetailTitle = pullRequestTitle
      pure ImportDetails {..}
    IssueUrl o r i -> do
      GitHub.Issue {..} <- errOrGitHub $ GitHub.issueR o r i
      let importDetailTitle = issueTitle
      pure ImportDetails {..}
