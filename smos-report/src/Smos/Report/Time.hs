{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Time where

import Autodocodec
import Control.Arrow
import Control.DeepSeq
import Data.Aeson
import Data.Function
import Data.Hashable
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Data.Validity
import Data.Void
import GHC.Generics (Generic)
import Numeric.Natural
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

data Time
  = Seconds Word
  | Minutes Word
  | Hours Word
  | Days Word
  | Weeks Word
  deriving stock (Show, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec Time)

instance Validity Time

instance Hashable Time

instance NFData Time

instance Eq Time where
  (==) = (==) `on` timeSeconds

instance Ord Time where
  compare = compare `on` timeSeconds

instance HasCodec Time where
  codec =
    dimapCodec f g $
      eitherCodec
        (bimapCodec parseTime renderTime codec Autodocodec.<?> "Time string: 2s, 3m, 4h, 5d, 6w, ...")
        (codec Autodocodec.<?> "Interpreted as a number of days")
    where
      f = \case
        Left t -> t
        Right w -> Days w
      g = Left

type P = Parsec Void Text

timeSeconds :: Time -> Natural
timeSeconds t =
  case t of
    Seconds i -> fromIntegral i
    Minutes i -> timeSeconds $ Seconds (60 * i)
    Hours i -> timeSeconds $ Minutes (60 * i)
    Days i -> timeSeconds $ Hours (24 * i)
    Weeks i -> timeSeconds $ Days (7 * i)

timeNominalDiffTime :: Time -> NominalDiffTime
timeNominalDiffTime = realToFrac . timeSeconds

time :: Text -> Maybe Time
time = parseMaybe timeP

parseTime :: Text -> Either String Time
parseTime = left errorBundlePretty . parse timeP "time string"

timeP :: P Time
timeP = do
  i <- decimal
  space
  c <- many asciiChar
  case c of
    "s" -> pure $ Seconds i
    "sec" -> pure $ Seconds i
    "secs" -> pure $ Seconds i
    "second" -> pure $ Seconds i
    "seconds" -> pure $ Seconds i
    "m" -> pure $ Minutes i
    "min" -> pure $ Minutes i
    "mins" -> pure $ Minutes i
    "minute" -> pure $ Minutes i
    "minutes" -> pure $ Minutes i
    "h" -> pure $ Hours i
    "hr" -> pure $ Hours i
    "hrs" -> pure $ Hours i
    "hour" -> pure $ Hours i
    "hours" -> pure $ Hours i
    "d" -> pure $ Days i
    "day" -> pure $ Days i
    "days" -> pure $ Days i
    "w" -> pure $ Weeks i
    "wk" -> pure $ Weeks i
    "wks" -> pure $ Weeks i
    "week" -> pure $ Weeks i
    "weeks" -> pure $ Weeks i
    _ -> fail $ "Unknown unit of time: " <> show c

renderTime :: Time -> Text
renderTime = T.pack . renderTimeString

renderTimeString :: Time -> String
renderTimeString t =
  let go singular plural i = if i == 1 then unwords ["1", singular] else unwords [show i, plural]
   in case t of
        Seconds i -> go "second" "seconds" i
        Minutes i -> go "minute" "minutes" i
        Hours i -> go "hour" "hours" i
        Days i -> go "day" "days" i
        Weeks i -> go "week" "weeks" i

renderTimeShort :: Time -> Text
renderTimeShort = T.pack . renderTimeStringShort

renderTimeStringShort :: Time -> String
renderTimeStringShort t =
  let go c i = show i <> [c]
   in case t of
        Seconds i -> go 's' i
        Minutes i -> go 'm' i
        Hours i -> go 'h' i
        Days i -> go 'd' i
        Weeks i -> go 'w' i
