{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.GitHub.Command.ImportSpec (spec) where

import Data.GenValidity.Time ()
import Data.Time
import Data.Validity.Path ()
import GitHub hiding (File)
import GitHub.Data.Name
import Path
import Smos.Data.TestUtils
import Smos.GitHub.Command.Import
import Smos.GitHub.Issue
import Smos.GitHub.Issue.Gen ()
import Smos.GitHub.OptParse
import Test.Syd
import Test.Syd.Validity

instance GenValid ImportDetails

spec :: Spec
spec = do
  describe "renderProjectPath" $ do
    it "produces valid paths" $
      producesValid
        renderProjectPath
    it "produces the same project path for this hypothetical issue" $
      case renderProjectPath
        ( PullRequestUrl
            (N "NorfairKing")
            (N "autodocodec")
            (IssueNumber 18)
        ) of
        Nothing -> expectationFailure "should have produced a project."
        Just smosFile ->
          pure $ pureGoldenStringFile "test_resources/project-path.txt" (fromRelFile smosFile)

  describe "resolveProjectPath" $ do
    let projectsDir = [absdir|/home/user/workflow/projects|]
        githubUrl = IssueUrl (N "NorfairKing") (N "validity") (IssueNumber 99)
    let pathFor = resolveProjectPath projectsDir githubUrl
    it "works for this issue without any destination" $
      pathFor
        ImportDestination
          { importDestinationFile = Nothing,
            importDestinationDirectory = Nothing
          }
        `shouldBe` Just [absfile|/home/user/workflow/projects/validity/issue-99.smos|]

    it "works for this issue with an absolute file destination without a directory destination" $
      pathFor
        ImportDestination
          { importDestinationFile = Just $ Path.Abs [absfile|/file.smos|],
            importDestinationDirectory = Nothing
          }
        `shouldBe` Just [absfile|/file.smos|]

    it "works for this issue with a relative file destination without a directory destination" $
      pathFor
        ImportDestination
          { importDestinationFile = Just $ Path.Rel [relfile|file.smos|],
            importDestinationDirectory = Nothing
          }
        `shouldBe` Just [absfile|/home/user/workflow/projects/file.smos|]

    it "works for this issue with an absolute file destination with a relative directory destination" $
      pathFor
        ImportDestination
          { importDestinationFile = Just $ Path.Abs [absfile|/file.smos|],
            importDestinationDirectory = Just $ Path.Rel [reldir|dir|]
          }
        `shouldBe` Just [absfile|/file.smos|]

    it "works for this issue with a relative file destination with a relative directory destination" $
      pathFor
        ImportDestination
          { importDestinationFile = Just $ Path.Rel [relfile|file.smos|],
            importDestinationDirectory = Just $ Path.Rel [reldir|dir|]
          }
        `shouldBe` Just [absfile|/home/user/workflow/projects/dir/file.smos|]

    it "works for this issue with an absolute file destination with an absolute directory destination" $
      pathFor
        ImportDestination
          { importDestinationFile = Just $ Path.Abs [absfile|/file.smos|],
            importDestinationDirectory = Just $ Path.Abs [absdir|/dir|]
          }
        `shouldBe` Just [absfile|/file.smos|]

    it "works for this issue with a relative file destination with an absolute directory destination" $
      pathFor
        ImportDestination
          { importDestinationFile = Just $ Path.Rel [relfile|file.smos|],
            importDestinationDirectory = Just $ Path.Abs [absdir|/dir|]
          }
        `shouldBe` Just [absfile|/dir/file.smos|]

    it "works for this issue without a file destination with a relative directory destination" $
      pathFor
        ImportDestination
          { importDestinationFile = Nothing,
            importDestinationDirectory = Just $ Path.Rel [reldir|dir|]
          }
        `shouldBe` Just [absfile|/home/user/workflow/projects/dir/validity/issue-99.smos|]

    it "works for this issue without a file destination with an absolute directory destination" $
      pathFor
        ImportDestination
          { importDestinationFile = Nothing,
            importDestinationDirectory = Just $ Path.Abs [absdir|/dir|]
          }
        `shouldBe` Just [absfile|/dir/validity/issue-99.smos|]

  describe "renderSmosProjects" $ do
    it "produces valid smosFiles" $
      forAllValid $
        producesValid3 . renderSmosProject

    let now = UTCTime (fromGregorian 2022 04 14) 36946
    it "produces the same smos file for this hypothetical issue" $
      case renderSmosProject
        now
        "https://github.com/NorfairKing/autodocodec/pull/18"
        ( PullRequestUrl
            (N "NorfairKing")
            (N "autodocodec")
            (IssueNumber 18)
        )
        (Just ImportDetails {importDetailTitle = "Disjoint Codecs"}) of
        Nothing -> expectationFailure "should have produced a project."
        Just smosFile ->
          pure $ pureGoldenSmosFile "test_resources/pull-request-with-details.smos" smosFile
    it "produces the same smos file for this hypothetical issue" $
      case renderSmosProject
        now
        "https://github.com/NorfairKing/autodocodec/pull/18"
        ( PullRequestUrl
            (N "NorfairKing")
            (N "autodocodec")
            (IssueNumber 18)
        )
        Nothing of
        Nothing -> expectationFailure "should have produced a project."
        Just smosFile ->
          pure $ pureGoldenSmosFile "test_resources/pull-request-without-details.smos" smosFile
