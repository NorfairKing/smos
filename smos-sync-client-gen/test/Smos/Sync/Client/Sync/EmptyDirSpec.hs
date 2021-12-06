{-# LANGUAGE OverloadedStrings #-}

module Smos.Sync.Client.Sync.EmptyDirSpec
  ( spec,
  )
where

import Control.Monad
import Control.Monad.Logger
import qualified Data.ByteString as SB
import Data.GenValidity.Path ()
import Path
import Path.IO
import Smos.Sync.Client.Command.Sync
import Test.Syd
import Test.Syd.Path
import Test.Syd.Validity

spec :: Spec
spec = tempDirSpec "smos-sync-client-empty-dirs" $ do
  it "leaves the contents dir" $ \tdir -> do
    runNoLoggingT $ removeEmptyDirs tdir
    doesDirExist tdir `shouldReturn` True

  it "leaves only nonempty directories" $ \tdir ->
    forAllValid $ \relFiles -> do
      forM_ (relFiles :: [Path Rel File]) $ \relFile -> do
        let file = tdir </> relFile
        ensureDir $ parent file
        SB.writeFile (fromAbsFile file) ""
      runNoLoggingT $ removeEmptyDirs tdir
      dirs <- fst <$> listDirRecur tdir
      forM_ dirs $ \dir -> do
        files <- snd <$> listDir tdir
        context (show dir) $ files `shouldBe` []

  it "works for this simple example" $ \tdir -> do
    emptyDir <- resolveDir tdir "empty"
    ensureDir emptyDir
    nonemptyDir <- resolveDir tdir "non-empty"
    ensureDir nonemptyDir
    file <- resolveFile nonemptyDir "file"
    SB.writeFile (fromAbsFile file) ""
    runNoLoggingT $ removeEmptyDirs tdir
    doesDirExist emptyDir `shouldReturn` False
    doesDirExist nonemptyDir `shouldReturn` True
