{-# LANGUAGE OverloadedStrings #-}

module Smos.GitHub.IssueSpec (spec) where

import GitHub
import GitHub.Data.Name
import Smos.GitHub.Issue
import Smos.GitHub.Issue.Gen ()
import Test.Syd

spec :: Spec
spec = do
  describe "parseGitHubUrl" $ do
    it "works for this pull request example" $
      parseGitHubUrl
        "https://github.com/owner/repo/pull/167"
        `shouldBe` Just
          ( PullRequestUrl
              (N "owner")
              (N "repo")
              (IssueNumber 167)
          )
    it "works for this issue example" $
      parseGitHubUrl
        "https://github.com/owner/repo/issues/168"
        `shouldBe` Just
          ( IssueUrl
              (N "owner")
              (N "repo")
              (IssueNumber 168)
          )
