{-# LANGUAGE QuasiQuotes #-}

module Smos.Docs.Site.Casts.TH where

import Control.Monad
import Instances.TH.Lift ()
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Path
import Path.IO
import Smos.Docs.Site.Constants
import System.Exit
import System.IO
import System.Process.Typed
import Yesod.EmbeddedStatic

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

mkCasts :: Q [Dec]
mkCasts = do
  specs <- runIO $ do
    cs <- resolveDir' "content/casts"
    fs <- snd <$> listDirRecur cs
    pure $ filter ((== Just ".yaml") . fileExtension) fs
  mapM_ (qAddDependentFile . fromAbsFile) specs
  tups <- runIO $ ensureCasts specs
  mapM_ (qAddDependentFile . snd) tups
  mkEmbeddedStatic development "casts" $ map (uncurry embedFileAt) tups

ensureCasts :: [Path Abs File] -> IO [(String, FilePath)]
ensureCasts specFiles = do
  hSetBuffering stdout LineBuffering -- To make sure the lines come out ok.
  hSetBuffering stderr LineBuffering
  mAutorecorderExecutable <- findExecutable [relfile|autorecorder|]
  forM specFiles $ \specFile -> do
    outputFile <- replaceExtension ".cast" specFile
    alreadyExists <- doesFileExist outputFile
    if alreadyExists
      then
        putStrLn $
          unwords
            [ "Not casting because the cast already exists:",
              fromAbsFile outputFile
            ]
      else do
        case mAutorecorderExecutable of
          Nothing -> die "The autorecorder executable is not found. You can install it from https://github.com/NorfairKing/autorecorder ."
          Just autorecorderExecutable -> do
            putStrLn $ "Casting " <> fromAbsFile specFile
            let cmd = fromAbsFile autorecorderExecutable
            let args = [fromAbsFile specFile, fromAbsFile outputFile, "--progress"]
            putStrLn $ unwords $ cmd : map show args
            runProcess_ $ setStderr inherit $ setStdout inherit $ proc cmd args
            putStrLn $ "Done casting " <> fromAbsFile specFile
    pure (fromRelFile (filename outputFile), fromAbsFile outputFile)
