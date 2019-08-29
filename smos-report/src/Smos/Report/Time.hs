{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Time where

import GHC.Generics (Generic)

import Data.Aeson
import Data.Function
import qualified Data.Text as T
import Data.Text (Text)
import Data.Validity
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char.Lexer

data Time
  = Seconds Word
  | Minutes Word
  | Hours Word
  | Days Word
  | Weeks Word
  deriving (Show, Generic)

instance Validity Time

instance Eq Time where
  (==) = (==) `on` timeSeconds

instance Ord Time where
  compare = compare `on` timeSeconds

instance FromJSON Time where
  parseJSON v =
    flip (withText "Time") v $ \t ->
      case parseTime t of
        Nothing -> fail "could not parse time."
        Just f -> pure f

instance ToJSON Time where
  toJSON = toJSON . renderTime

type P = Parsec Void Text

timeSeconds :: Time -> Word
timeSeconds t =
  case t of
    Seconds i -> i
    Minutes i -> timeSeconds $ Seconds (60 * i)
    Hours i -> timeSeconds $ Minutes (60 * i)
    Days i -> timeSeconds $ Hours (24 * i)
    Weeks i -> timeSeconds $ Days (7 * i)

parseTime :: Text -> Maybe Time
parseTime = parseMaybe timeP

timeP :: P Time
timeP = do
  i <- decimal
  c <- charLiteral
  case c of
    's' -> pure $ Seconds i
    'm' -> pure $ Minutes i
    'h' -> pure $ Hours i
    'd' -> pure $ Days i
    'w' -> pure $ Weeks i
    _ -> fail $ "Unknown unit of time: " <> show c

renderTime :: Time -> Text
renderTime = T.pack . renderTimeString

renderTimeString :: Time -> String
renderTimeString t =
  let go c i = show i <> [c]
   in case t of
        Seconds i -> go 's' i
        Minutes i -> go 'm' i
        Hours i -> go 'h' i
        Days i -> go 'd' i
        Weeks i -> go 'w' i
