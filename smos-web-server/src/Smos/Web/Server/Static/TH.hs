module Smos.Web.Server.Static.TH
  ( mkStatic,
  )
where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Path
import Path.IO
import Smos.Web.Server.Constants
import System.Environment (lookupEnv)
import Yesod.EmbeddedStatic
import Yesod.EmbeddedStatic.Remote

mkStatic :: Q [Dec]
mkStatic = do
  md <- runIO $ lookupEnv "SMOS_CASTS"
  castTups <- case md of
    Nothing -> do
      runIO $ putStrLn "WARNING: Building without casts. Set SMOS_CASTS to build them during development."
      pure []
    Just castsDir -> do
      castFiles <- runIO $ do
        cs <- resolveDir' castsDir
        snd <$> listDirRecur cs
      mapM_ (qAddDependentFile . fromAbsFile) castFiles
      pure $ map (\f -> ("casts_" <> fromRelFile (filename f), fromAbsFile f)) castFiles

  runIO $ print castTups
  let remoteStatic fp = embedRemoteFileAt fp ("static/" ++ fp)
  mkEmbeddedStatic
    development
    "smosWebServerStatic"
    $ [ remoteStatic "favicon.ico" "https://cs-syd.eu/logo/res/favicon.ico",
        remoteStatic "jquery.js" "https://code.jquery.com/jquery-3.7.1.min.js",
        remoteStatic "xterm.js" "https://cdn.jsdelivr.net/npm/xterm@5.3.0/lib/xterm.min.js",
        remoteStatic "xterm.css" "https://cdn.jsdelivr.net/npm/xterm@5.3.0/css/xterm.css",
        remoteStatic "xterm-attach.js" "https://cdn.jsdelivr.net/npm/xterm-addon-attach@0.9.0/lib/xterm-addon-attach.min.js",
        remoteStatic "xterm-fit.js" "https://cdn.jsdelivr.net/npm/xterm-addon-fit@0.8.0/lib/xterm-addon-fit.min.js",
        remoteStatic "asciinema-player.js" "https://cdn.jsdelivr.net/npm/asciinema-player@3.7.1/dist/bundle/asciinema-player.min.js",
        remoteStatic "asciinema-player.css" "https://cdn.jsdelivr.net/npm/asciinema-player@3.7.1/dist/bundle/asciinema-player.css",
        remoteStatic "bulma-carousel.js" "https://cdn.jsdelivr.net/npm/bulma-carousel@4.0.24/dist/js/bulma-carousel.min.js",
        embedDir "assets"
      ]
      ++ map (uncurry embedFileAt) castTups
