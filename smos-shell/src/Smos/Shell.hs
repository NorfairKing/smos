{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Shell
  ( smosShell,
    smosShellWith,
  )
where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.ByteString as SB
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Options.Applicative as OptParse
import Rainbow
import qualified Smos.Query as Query
import qualified Smos.Query.Default as Query
import qualified Smos.Query.OptParse as Query
import qualified Smos.Report.OptParse as Report
import System.Console.Haskeline as Haskeline
import System.Console.Haskeline.Command.KillRing as Haskeline
import System.Console.Haskeline.History as Haskeline
import System.Console.Haskeline.InputT as Haskeline
import System.Console.Haskeline.Term as Haskeline
import System.Exit
import System.IO

smosShell :: IO ()
smosShell = smosShellWith Query.defaultReportConfig stdin stdout stderr

-- TODO use a config based on actual optparse
smosShellWith :: Query.SmosReportConfig -> Handle -> Handle -> Handle -> IO ()
smosShellWith rc inputH outputH errorH = do
  colouredBsMaker <- byteStringMakerFromHandle outputH
  let prependECSign :: Maybe ExitCode -> [Chunk] -> [Chunk]
      prependECSign = \case
        Just (ExitFailure _) -> (fore red (chunk "ERR ") :)
        Just ExitSuccess -> (fore green (chunk "OK! ") :)
        Nothing -> id
      makePrompt :: Maybe ExitCode -> [Chunk]
      makePrompt mex = prependECSign mex [fore white $ chunk "smos-query > "]
      renderChunks = T.unpack . TE.decodeUtf8 . SB.concat . chunksToByteStrings colouredBsMaker
      loop :: Maybe ExitCode -> InputT IO ()
      loop mex = do
        let prompt = renderChunks $ makePrompt mex
        minput <- getInputLine prompt
        case words <$> minput of
          Nothing -> pure ()
          Just ["exit"] -> pure ()
          Just ["quit"] -> pure ()
          Just ["help"] -> do
            outputStrLn "Try running query --help to see an overview of the reports you can run."
            loop Nothing
          Just [] -> loop Nothing
          Just input -> do
            case OptParse.execParserPure OptParse.defaultPrefs Query.argParser input of
              OptParse.Failure failure -> do
                let (renderedError, exitCode) = OptParse.renderFailure failure progName
                case exitCode of
                  ExitSuccess -> outputStrLn renderedError
                  ExitFailure _ -> liftIO $ hPutStrLn errorH renderedError
                loop (Just exitCode)
              OptParse.CompletionInvoked completion -> do
                msg <- liftIO $ OptParse.execCompletion completion progName
                outputStrLn msg -- TODO not sure what to do with this yet.
                loop Nothing
              OptParse.Success (Query.Arguments cmd flags) -> do
                ec <- liftIO $ do
                  instructions <-
                    liftIO $
                      Query.combineToInstructions
                        ( Query.SmosQueryConfig
                            { Query.smosQueryConfigReportConfig = rc,
                              Query.smosQueryConfigColourConfig = Query.defaultColourConfig,
                              Query.smosQueryConfigInputHandle = inputH,
                              Query.smosQueryConfigOutputHandle = outputH,
                              Query.smosQueryConfigErrorHandle = errorH
                            }
                        )
                        cmd
                        (Report.flagWithRestFlags flags)
                        Query.emptyEnvironment
                        Nothing
                  -- TODO Catch synchronous exceptions too.
                  (ExitSuccess <$ Query.smosQueryWithInstructions instructions) `catch` (\ec -> pure ec)
                loop (Just ec)
  customRunInputT $ loop Nothing
  where
    -- TODO try to use the special TTY handles instead so history works too.
    customRunInputT :: InputT IO a -> IO a
    customRunInputT inputT = do
      historyRef <- newIORef Haskeline.emptyHistory
      withBehavior (useFileHandle inputH) $ \rt -> do
        let runTerm =
              rt
                { putStrOut = \s -> do
                    hPutStr outputH s
                    hFlush outputH
                }
        runReaderT
          ( runReaderT
              ( Haskeline.runKillRing
                  ( runReaderT
                      ( runReaderT (unInputT inputT) runTerm
                      )
                      historyRef
                  )
              )
              Haskeline.defaultPrefs
          )
          Haskeline.defaultSettings
    progName :: String
    progName = "query"
