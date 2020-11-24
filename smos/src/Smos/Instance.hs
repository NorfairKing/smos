module Smos.Instance where

import Conduit
import qualified Graphics.Vty as Vty (Config (..), defaultConfig, mkVty)
import Graphics.Vty.Output.TerminfoBased (setWindowSize)
import Smos
import Smos.Terminal
import Smos.Types

withSmosInstance :: MonadUnliftIO m => SmosConfig -> Maybe StartingPath -> (TerminalHandle -> m a) -> m a
withSmosInstance config mStartingPath = withTerminal $ \_ slaveFd ->
  let vtyBuilder = do
        vty <-
          Vty.mkVty $
            Vty.defaultConfig
              { Vty.inputFd = Just slaveFd,
                Vty.outputFd = Just slaveFd
              }
        setWindowSize slaveFd (80, 24)
        pure vty
      runSmos :: MonadIO m => m ()
      runSmos = liftIO $ startSmosWithVtyBuilderOn vtyBuilder mStartingPath config
   in runSmos
