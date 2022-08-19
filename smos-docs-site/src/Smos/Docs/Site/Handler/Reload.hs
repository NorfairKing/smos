module Smos.Docs.Site.Handler.Reload
  ( getReloadR,
  )
where

import Control.Concurrent
import Control.Monad
import Data.List
import Path
import Path.IO
import Smos.Docs.Site.Handler.Import
import System.FSNotify as Notify
import Yesod.AutoReload

getReloadR :: Handler ()
getReloadR = getAutoReloadRWith $
  liftIO $ do
    sendRefreshVar <- newEmptyMVar -- A variable to block on
    Notify.withManager $ \mgr -> do
      let predicate e = case e of
            -- Don't watch removed events, in case the file is rewritten, so we don't get a 404 when reconecting
            Removed {} -> False
            _ ->
              let suffixes =
                    [ ".swp",
                      "~",
                      ".swx",
                      ".ch.swpx",
                      "4913" -- https://github.com/neovim/neovim/issues/3460
                    ] -- Editors make files like this, no need to refresh when they are written.
               in not $ any (`isSuffixOf` eventPath e) suffixes
          act e = putMVar sendRefreshVar e
      let dirs = ["content", "demo-workflow", "static"]
      forM_ dirs $ \d -> do
        ad <- resolveDir' d
        watchTree mgr (fromAbsDir ad) predicate act
      putStrLn "Waiting for a file to change."
      _ <- takeMVar sendRefreshVar
      pure ()
