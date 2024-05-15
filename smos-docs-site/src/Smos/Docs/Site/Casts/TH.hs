module Smos.Docs.Site.Casts.TH where

import Instances.TH.Lift ()
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Path
import Path.IO
import Smos.Docs.Site.Constants
import System.Environment (lookupEnv)
import Yesod.EmbeddedStatic

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

mkCasts :: Q [Dec]
mkCasts = do
  md <- runIO $ lookupEnv "SMOS_DOCS_CASTS"
  tups <- case md of
    Nothing -> do
      runIO $ putStrLn "WARNING: Building without casts. Set SMOS_DOCS_CASTS to build them during development."
      pure []
    Just castsDir -> do
      castFiles <- runIO $ do
        cs <- resolveDir' castsDir
        fs <- snd <$> listDirRecur cs
        pure $ filter ((== Just ".cast") . fileExtension) fs
      mapM_ (qAddDependentFile . fromAbsFile) castFiles
      pure $ map (\f -> (fromRelFile (filename f), fromAbsFile f)) castFiles

  mapM_ (qAddDependentFile . snd) tups
  mkEmbeddedStatic development "casts" $ map (uncurry embedFileAt) tups
