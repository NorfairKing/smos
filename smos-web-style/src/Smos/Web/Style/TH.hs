module Smos.Web.Style.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Path
import Path.IO
import Smos.Web.Style.Constants
import System.Environment
import System.Exit
import System.Process
import Yesod.EmbeddedStatic

mkStyle :: Q [Dec]
mkStyle = do
  md <- runIO $ lookupEnv "STYLE_FILE"
  embeddedStyle <- case md of
    Nothing -> do
      d <- resolveDir' "style"
      runIO $ putStrLn $ unwords ["WARNING: Including style files from dir at path: ", fromAbsDir d]
      scssFile <- resolveFile d "mybulma.scss"
      cssFile <- resolveFile d "mybulma.css"
      qAddDependentFile $ fromAbsFile scssFile
      runIO $ do
        putStrLn $ unwords ["Regenerating the stylesheet at", fromAbsFile cssFile]
        ec <-
          system $
            unwords
              [ "sass",
                "--sourcemap=none",
                concat [fromAbsFile scssFile, ":", fromAbsFile cssFile],
                if development then "" else "--style compressed"
              ]
        case ec of
          ExitSuccess -> pure ()
          ExitFailure i -> die $ "Regenerating the stylesheet failed with exit code: " <> show i
      pure $ embedFileAt "index.css" $ fromAbsFile cssFile
    Just f -> do
      runIO $ putStrLn $ unwords ["Including style in:", f]
      pure $ embedFileAt "index.css" f

  mkEmbeddedStatic
    development
    "smosWebStyle"
    [ embeddedStyle
    ]
