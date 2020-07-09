module Smos.ASCIInemaSpec (spec) where

import Control.Monad
import Data.List
import Path
import Path.IO
import Smos.ASCIInema
import System.Environment
import Test.Hspec

spec :: Spec
spec = do
  fs <- runIO $ do
    examplesDir <- resolveDir' "examples"
    castsDir <- resolveDir' "casts"
    relFiles <- filter ((== ".yaml") . fileExtension) . filter (not . hidden) . snd <$> listDirRecurRel examplesDir
    forM relFiles $ \relFile -> do
      let inFile = examplesDir </> relFile
      outFile <- setFileExtension ".cast" $ castsDir </> relFile
      pure (inFile, outFile)
  forM_ fs $ \(fi, fo) ->
    it ("'Just works' for this example: " <> fromAbsFile fi) $ do
      let args = ["record", fromAbsFile fi, fromAbsFile fo, "--wait", "0.5"]
      putStrLn $ "Running: asciinema " <> unwords (map show args)
      withArgs args smosASCIInema

hidden :: Path r File -> Bool
hidden f = ".swp" `isSuffixOf` toFilePath f
