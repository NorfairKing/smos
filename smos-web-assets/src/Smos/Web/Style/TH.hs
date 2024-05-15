module Smos.Web.Style.TH where

import Language.Haskell.TH
import Smos.Web.Style.Constants
import System.Environment
import System.Exit
import Yesod.EmbeddedStatic

mkStyle :: Q [Dec]
mkStyle = do
  md <- runIO $ lookupEnv "SMOS_STYLE"
  embeddedStyle <- case md of
    Nothing -> runIO $ die "Could not embed style file."
    Just f -> do
      runIO $ putStrLn $ unwords ["Including style in:", f]
      pure $ embedFileAt "index.css" f

  mkEmbeddedStatic
    development
    "smosWebStyle"
    [ embeddedStyle
    ]
