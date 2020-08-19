{-# OPTIONS_GHC -ddump-splices #-}

module Smos.Docs.Site.Casts.TH where

import Instances.TH.Lift ()
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Path
import Path.IO
import Smos.ASCIInema.Commands.Record as ASCIInema
import Smos.ASCIInema.Input as ASCIInema
import Smos.ASCIInema.OptParse.Types as ASCIInema
import Smos.ASCIInema.Output as ASCIInema
import Smos.Docs.Site.Constants
import Yesod.EmbeddedStatic

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

mkCasts :: Q [Dec]
mkCasts = do
  specs <- runIO $ do
    cs <- resolveDir' "content/casts"
    fs <- snd <$> listDirRecur cs
    pure $ filter ((== Just ".yaml") . fileExtension) fs
  mapM_ (qAddDependentFile . fromAbsFile) specs
  tups <- runIO $ mapM ensureCast specs
  mapM_ (qAddDependentFile . snd) tups
  mkEmbeddedStatic development "casts" $ map (uncurry embedFileAt) tups

ensureCast :: Path Abs File -> IO (String, FilePath)
ensureCast specFile = do
  outputFile <- replaceExtension ".cast" specFile
  alreadyExists <- doesFileExist outputFile
  if alreadyExists
    then putStrLn $ unwords ["Not casting because the cast already exists:", fromAbsFile outputFile]
    else do
      putStrLn $ "Casting " <> fromAbsFile specFile
      let sets =
            RecordSettings
              { recordSetSpecFile = specFile,
                recordSetOutputFile = outputFile,
                recordSetWait = 1,
                recordSetColumns = 80,
                recordSetRows = 25,
                recordSetMistakes = NoMistakes, -- TODO make the mistakes work: MistakesWithProbability 0.03,
                recordSetOutputView = ProgressOutputView -- DebugOutputView
              }
      ASCIInema.record sets
  pure (fromRelFile (filename outputFile), fromAbsFile outputFile)
