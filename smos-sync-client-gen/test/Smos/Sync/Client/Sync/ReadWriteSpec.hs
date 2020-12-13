{-# LANGUAGE OverloadedStrings #-}

module Smos.Sync.Client.Sync.ReadWriteSpec
  ( spec,
  )
where

import Control.Monad.Logger
import qualified Data.ByteString as SB
import Data.GenValidity.ByteString ()
import Data.GenValidity.Path ()
import Path
import Path.IO
import Smos.Sync.Client.Command.Sync
import Test.Syd

import Test.Syd.Validity

spec :: Spec
spec =
  -- Writing files takes a while
  modifyMaxSuccess (`div` 10) $
    modifyMaxShrinks (const 0) $
      modifyMaxSize (const 1) $ do
        describe "readFileSafely" $ do
          it "reads a file that was just written" $
            forAllValid $ \rf ->
              forAllValid $ \contents ->
                withSystemTempDir "smos-sync-client-contents" $ \tdir -> do
                  let af = tdir </> rf
                  SB.writeFile (fromAbsFile af) contents
                  contents' <- readFileSafely af
                  contents' `shouldBe` Just contents
          it "reads Nothing if there is no file" $
            forAllValid $ \rf ->
              withSystemTempDir "smos-sync-client-contents" $ \tdir -> do
                let af = tdir </> rf
                contents' <- readFileSafely af
                contents' `shouldBe` Nothing
          it "reads Nothing if there is no a dir" $
            forAllValid $ \rf ->
              withSystemTempDir "smos-sync-client-contents" $ \tdir -> do
                let af = tdir </> rf
                case parseAbsDir (fromAbsFile af) of
                  Nothing -> pure ()
                  Just ad -> do
                    ensureDir ad
                    contents' <- readFileSafely af
                    contents' `shouldBe` Nothing
        describe "writeFileSafely" $ do
          it "writes over an existent file" $
            forAllValid $ \rf ->
              forAllValid $ \contents1 ->
                forAllValid $ \contents2 ->
                  withSystemTempDir "smos-sync-client-contents" $ \cdir -> do
                    withSystemTempDir "smos-sync-client-backup" $ \bdir -> do
                      let af = cdir </> rf
                      runStderrLoggingT $ do
                        writeFileSafely cdir bdir rf contents1
                        writeFileSafely cdir bdir rf contents2
                      contents' <- readFileSafely af
                      contents' `shouldBe` Just contents2
          it "writes over an existent dir" $
            forAllValid $ \rf ->
              forAllValid $ \contents ->
                withSystemTempDir "smos-sync-client-contents" $ \cdir -> do
                  withSystemTempDir "smos-sync-client-backup" $ \bdir -> do
                    let af = cdir </> rf
                    case parseAbsDir (fromAbsFile af) of
                      Nothing -> pure ()
                      Just ad -> do
                        ensureDir ad
                        runStderrLoggingT $ writeFileSafely cdir bdir rf contents
                        contents' <- readFileSafely af
                        contents' `shouldBe` Just contents
          it "writes over an existent dir with contents" $
            forAllValid $ \rf1 ->
              forAllValid $ \rf2 ->
                forAllValid $ \contents1 ->
                  forAllValid $ \contents2 ->
                    withSystemTempDir "smos-sync-client-contents" $ \cdir -> do
                      withSystemTempDir "smos-sync-client-backup" $ \bdir -> do
                        let af1 = cdir </> rf1
                        case parseRelDir (fromRelFile rf1) of
                          Nothing -> pure ()
                          Just rd -> do
                            let rf2' = rd </> rf2
                            runStderrLoggingT $ do
                              writeFileSafely cdir bdir rf2' contents2
                              writeFileSafely cdir bdir rf1 contents1
                            contents' <- readFileSafely af1
                            contents' `shouldBe` Just contents1
          it "writes over an existent file higher up" $
            forAllValid $ \rf1 ->
              forAllValid $ \rf2 ->
                forAllValid $ \contents1 ->
                  forAllValid $ \contents2 ->
                    withSystemTempDir "smos-sync-client-contents" $ \cdir -> do
                      withSystemTempDir "smos-sync-client-backup" $ \bdir -> do
                        case parseRelDir (fromRelFile rf1) of
                          Nothing -> pure ()
                          Just rd -> do
                            let rf2' = rd </> rf2
                            runStderrLoggingT $ do
                              writeFileSafely cdir bdir rf1 contents1
                              writeFileSafely cdir bdir rf2' contents2
                            let af2 = cdir </> rf2'
                            contents' <- readFileSafely af2
                            contents' `shouldBe` Just contents2
          it "writes over an existent file higher up, two levels" $
            forAllValid $ \rf1 ->
              forAllValid $ \rd2 ->
                forAllValid $ \rf2 ->
                  forAllValid $ \contents1 ->
                    forAllValid $ \contents2 ->
                      withSystemTempDir "smos-sync-client-contents" $ \cdir -> do
                        withSystemTempDir "smos-sync-client-backup" $ \bdir -> do
                          case parseRelDir (fromRelFile rf1) of
                            Nothing -> pure ()
                            Just rd -> do
                              let rf2' = rd </> rd2 </> rf2
                              runStderrLoggingT $ do
                                writeFileSafely cdir bdir rf1 contents1
                                writeFileSafely cdir bdir rf2' contents2
                              let af2 = cdir </> rf2'
                              contents' <- readFileSafely af2
                              contents' `shouldBe` Just contents2
