{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.ASCIInema.Input where

import Conduit
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Char as Char
import qualified Data.Conduit.Combinators as C
import Data.Conduit.List (sourceList)
import Data.Maybe
import Data.Random.Normal
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Data.Time
import Smos.ASCIInema.Output
import Smos.ASCIInema.Spec
import System.IO
import System.Posix.Terminal
import System.Random
import Text.Printf

type Speed = Double

data Mistakes
  = NoMistakes
  | MistakesWithProbability Double
  deriving (Show, Eq)

inputWriter :: MonadIO m => OutputView -> Speed -> Mistakes -> TerminalAttributes -> Handle -> [ASCIInemaCommand] -> ConduitT () void m [(UTCTime, Text)]
inputWriter ov speed mistakes attrs handle commands =
  ( \ic -> case ov of
      DebugOutputView -> sourceList commands .| ic .| inputDebugConduit
      ProgressOutputView -> inputListProgressConduit commands .| ic
      _ -> sourceList commands .| ic
  )
    (inputConduit speed mistakes attrs)
    .| inputRecorder `fuseUpstream` C.map TE.encodeUtf8 `fuseUpstream` sinkHandle handle

inputRecorder :: MonadIO m => ConduitT i i m [(UTCTime, i)]
inputRecorder = go []
  where
    go acc = do
      mi <- await
      case mi of
        Nothing -> pure acc
        Just i -> do
          now <- liftIO getCurrentTime
          yield i
          go $ (now, i) : acc

inputDebugConduit :: MonadIO m => ConduitT Text Text m ()
inputDebugConduit = C.mapM $ \t -> do
  liftIO $ T.putStrLn $ "Sending input: " <> T.pack (show t)
  pure t

inputListProgressConduit :: MonadIO m => [ASCIInemaCommand] -> ConduitT i ASCIInemaCommand m ()
inputListProgressConduit as = foldM_ go 0 as
  where
    totalDelay = sum $ map commandDelay as
    adjust = (`div` 100)
    lenStrLen = length (show (adjust totalDelay))
    showIntWithLen :: Word -> String
    showIntWithLen w = printf ("%" <> show lenStrLen <> "d") (adjust w)
    progressStr i = concat ["Progress: [", showIntWithLen i, "/", showIntWithLen totalDelay, "]"]
    go :: MonadIO m => Word -> ASCIInemaCommand -> ConduitT i ASCIInemaCommand m Word
    go currentTiming c = do
      yield c
      let newTiming = currentTiming + commandDelay c
      unless (currentTiming == newTiming) $ liftIO $ putStrLn $ progressStr newTiming
      pure newTiming

inputConduit :: MonadIO m => Speed -> Mistakes -> TerminalAttributes -> ConduitT ASCIInemaCommand Text m ()
inputConduit speed mistakes attrs = awaitForever go
  where
    go :: MonadIO m => ASCIInemaCommand -> ConduitT ASCIInemaCommand Text m ()
    go = \case
      Wait i -> liftIO $ waitMilliSeconds speed i
      SendInput s -> yield $ T.pack $ map mapChar s
      Type s i -> typeString s i
    mapChar :: Char -> Char
    mapChar c = case c of
      '\n' -> fromMaybe c $ controlChar attrs EndOfLine
      '\b' -> fromMaybe c $ controlChar attrs Erase
      _ -> c
    typeString s i =
      forM_ s $ \c -> do
        randomMistake <- liftIO decideToMakeAMistake
        when randomMistake $ makeAMistake c
        waitForChar c
        go $ SendInput [c]
      where
        decideToMakeAMistake :: IO Bool
        decideToMakeAMistake =
          case mistakes of
            NoMistakes -> pure False
            MistakesWithProbability p -> do
              -- Make a mistake with likelihood p
              let accuracy = 1000 :: Int
              randomNum <- randomRIO (0, accuracy)
              pure $ randomNum < round (p * fromIntegral accuracy)
        makeAMistake :: MonadIO m => Char -> ConduitT ASCIInemaCommand Text m ()
        makeAMistake c = do
          let possibleMistakes = validMistakes c
          randomIndex <- liftIO $ randomRIO (0, length possibleMistakes - 1)
          let c' = possibleMistakes !! randomIndex
          waitForChar c'
          go $ SendInput [c']
          waitForChar '\b'
          go $ SendInput ['\b'] -- Backspace
        waitForChar :: MonadIO m => Char -> ConduitT ASCIInemaCommand Text m ()
        waitForChar c = do
          randomDelay <- liftIO $ normalIO' (0, 25) -- Add some random delay to make the typing feel more natural
          go $ Wait $ round ((fromIntegral i * charSpeed c) + randomDelay :: Double)

-- | Add a delay multiplier based on what kind of character it is to make the typing feel more natural.
charSpeed :: Char -> Double
charSpeed ' ' = 1.25
charSpeed '\b' = 3 -- It takes a while to notice a mistake
charSpeed c
  | c `elem` ['a' .. 'z'] = 0.75
  | c `elem` ['A' .. 'Z'] = 1.5 -- Because you have to press 'shift'
  | otherwise = 2 -- Special characters take even longer

waitMilliSeconds :: Double -> Word -> IO ()
waitMilliSeconds speed delay = threadDelay $ round $ fromIntegral (delay * 1000) / speed

validMistakes :: Char -> [Char]
validMistakes c =
  if Char.isUpper c -- You won't accidentally type an upper-case character if the character you intended was lower-case
    then
      concat
        [ ['A' .. 'Z'],
          "[{+(=*)!}]" -- Assuming a keyboard layout where shift gives you punctuation, like qwerty
        ]
    else
      concat
        [ ['a' .. 'z'],
          ['0' .. '9']
        ]
