module Smos.Web.Assets.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Path
import Path.IO
import Smos.Web.Assets.Constants
import System.Environment
import System.Exit
import Yesod.EmbeddedStatic

mkAssets :: Q [Dec]
mkAssets = do
  md <- runIO $ lookupEnv "SMOS_STYLE"
  embeddedStyle <- case md of
    Nothing -> runIO $ die "Could not embed style file."
    Just f -> do
      runIO $ putStrLn $ unwords ["Including style in:", f]
      pure $ embedFileAt "index.css" f

  ad <- runIO $ lookupEnv "SMOS_CASTS"
  assetsTups <- case ad of
    Nothing -> do
      runIO $ putStrLn "WARNING: Building without casts. Set SMOS_CASTS to build them during development."
      pure []
    Just castsDir -> do
      castFiles <- runIO $ do
        cs <- resolveDir' castsDir
        snd <$> listDirRecur cs
      mapM_ (qAddDependentFile . fromAbsFile) castFiles
      pure $ map (\f -> ("casts_" <> fromRelFile (filename f), fromAbsFile f)) castFiles

  mkEmbeddedStatic
    development
    "smosWebAssets"
    (embeddedStyle : map (uncurry embedFileAt) assetsTups)
