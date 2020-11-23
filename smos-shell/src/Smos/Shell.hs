module Smos.Shell
  ( smosShell,
  )
where

import Control.Exception as Exception
import Control.Monad.IO.Class
import qualified Options.Applicative as OptParse
import qualified Smos.Query as Query
import qualified Smos.Query.Default as Query
import qualified Smos.Query.OptParse as Query
import qualified Smos.Report.OptParse as Report
import System.Console.Haskeline
import System.Exit

smosShell :: IO ()
smosShell = smosShellWith Query.defaultReportConfig

smosShellWith :: Query.SmosReportConfig -> IO ()
smosShellWith rc = runInputT defaultSettings $ loop Nothing
  where
    loop :: Maybe ExitCode -> InputT IO ()
    loop mex = do
      let prompt = case mex of
            Just (ExitFailure _) -> "✖ smos > "
            Just ExitSuccess -> "✔ smos > "
            Nothing -> "smos > "
      minput <- getInputLine prompt
      case words <$> minput of
        Nothing -> pure ()
        Just ["exit"] -> pure ()
        Just ["quit"] -> pure ()
        Just [] -> loop Nothing
        Just ("query" : input) -> do
          errOrExit <-
            liftIO
              ( (Right <$> OptParse.handleParseResult (OptParse.execParserPure OptParse.defaultPrefs Query.argParser input))
                  `Exception.catch` (pure . Left)
              )
          case errOrExit of
            Left exitCode -> loop $ Just exitCode
            Right (Query.Arguments cmd flags) -> do
              liftIO $ do
                instructions <-
                  liftIO $
                    Query.combineToInstructions
                      (Query.defaultSmosQueryConfig {Query.smosQueryConfigReportConfig = rc})
                      cmd
                      (Report.flagWithRestFlags flags)
                      Query.emptyEnvironment
                      Nothing
                Query.smosQueryWithInstructions instructions
              loop (Just ExitSuccess)
        Just (cmd : _) -> do
          outputStrLn $ "Command not recognised: " <> cmd
          loop Nothing
