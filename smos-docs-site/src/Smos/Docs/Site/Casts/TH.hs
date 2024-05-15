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
  md <- runIO $ lookupEnv "SMOS_CASTS"
  tups <- case md of
    Nothing -> do
      runIO $ putStrLn "WARNING: Building without casts. Set SMOS_CASTS to build them during development."
      pure []
    Just castsDir -> do
      castFiles <- runIO $ do
        cs <- resolveDir' castsDir
        snd <$> listDirRecur cs
      mapM_ (qAddDependentFile . fromAbsFile) castFiles
      pure $ map (\f -> ("casts_" <> fromRelFile (filename f), fromAbsFile f)) castFiles

  mkEmbeddedStatic development "casts" $ map (uncurry embedFileAt) tups
