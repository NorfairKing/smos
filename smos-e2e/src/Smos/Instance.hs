module Smos.Instance where

import Conduit
import qualified Graphics.Vty as Vty (defaultConfig, mkVtyFromPair)
import qualified Graphics.Vty.Platform.Unix.Input as Vty (buildInput)
import qualified Graphics.Vty.Platform.Unix.Output as Vty (buildOutput)
import qualified Graphics.Vty.Platform.Unix.Output.TerminfoBased as Vty (setWindowSize)
import qualified Graphics.Vty.Platform.Unix.Settings as Vty (UnixSettings (..), defaultSettings)
import Smos
import Smos.Terminal
import Smos.Types

withSmosInstance :: (MonadUnliftIO m) => SmosConfig -> Maybe StartingPath -> (TerminalHandle -> m a) -> m a
withSmosInstance config mStartingPath = withTerminal $ \_ slaveFd ->
  let vtyBuilder = do
        let vtyConfig = Vty.defaultConfig
        unixSettings <- (\s -> s {Vty.settingInputFd = slaveFd, Vty.settingOutputFd = slaveFd}) <$> Vty.defaultSettings
        input <- Vty.buildInput vtyConfig unixSettings
        output <- Vty.buildOutput vtyConfig unixSettings
        vty <- Vty.mkVtyFromPair input output
        Vty.setWindowSize slaveFd (80, 24)
        pure vty
      runSmos :: (MonadIO m) => m ()
      runSmos = liftIO $ startSmosWithVtyBuilderOn vtyBuilder mStartingPath config
   in runSmos
