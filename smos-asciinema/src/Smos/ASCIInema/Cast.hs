{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.ASCIInema.Cast where

import Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Time

data Cast = Cast {castHeader :: Header, castEvents :: [Event]}
  deriving (Show, Eq)

renderCast :: Cast -> LB.ByteString
renderCast Cast {..} =
  LB8.unlines $ JSON.encode castHeader : map JSON.encode castEvents

parseCast :: LB.ByteString -> Either String Cast
parseCast bs =
  let ls = LB8.lines bs
   in case ls of
        [] -> Left "Expected at least one line but got none"
        (h : es) -> do
          castHeader <- JSON.eitherDecode h
          castEvents <- mapM JSON.eitherDecode es
          pure Cast {..}

data Header
  = Header
      { headerWidth :: Word,
        headerHeight :: Word,
        headerStartTimestamp :: Maybe UTCTime,
        headerDuration :: Maybe Double,
        headerIdleTimeLimit :: Maybe Double,
        headerCommand :: Maybe String,
        headerTitle :: Maybe Text,
        headerEnv :: Maybe (Map String String)
        -- castTheme :: Maybe ColourTheme
      }
  deriving (Show, Eq)

instance FromJSON Header where
  parseJSON =
    withObject "Header" $ \o ->
      Header
        <$> o .: "width"
        <*> o .: "height"
        <*> ((fmap (show :: Int -> String) <$> o .:? "timestamp") >>= traverse (parseTimeM False defaultTimeLocale "%s"))
        <*> o .:? "duration"
        <*> o .:? "idle_time_limit"
        <*> o .:? "command"
        <*> o .:? "title"
        <*> o .:? "env"

instance ToJSON Header where
  toJSON Header {..} =
    object $
      concat
        [ [ "version" .= (2 :: Word),
            "width" .= headerWidth,
            "height" .= headerHeight
          ],
          ["timestamp" .= formatTime defaultTimeLocale "%s" ts | ts <- maybeToList headerStartTimestamp],
          ["duration" .= d | d <- maybeToList headerDuration],
          ["idle_time_limit" .= itl | itl <- maybeToList headerIdleTimeLimit],
          ["command" .= c | c <- maybeToList headerCommand],
          ["title" .= t | t <- maybeToList headerTitle],
          ["env" .= e | e <- maybeToList headerEnv]
        ]

-- -- Let's not mess with colours yet
--
-- data ColourTheme
--   = ColourTheme
--       { themeForeground :: Colour,
--         themeBackground :: Colour,
--         themePalette :: ColourPalette
--       }
--   deriving (Show, Eq)
--
-- type Colour = Text -- #d0d0d0 format
--
-- data ColourPalette
--   = Palette8 Colour Colour Colour Colour Colour Colour Colour Colour
--   | Palette16 Colour Colour Colour Colour Colour Colour Colour Colour Colour Colour Colour Colour Colour Colour Colour Colour
--   deriving (Show, Eq)

data Event
  = Event
      { eventTime :: Double,
        eventData :: EventData
      }
  deriving (Show, Eq)

data EventData
  = EventInput Text
  | EventOutput Text
  deriving (Show, Eq)

instance ToJSON Event where
  toJSON Event {..} =
    let (typ, dat) = case eventData of
          EventInput t -> ("i" :: Text, t)
          EventOutput t -> ("o", t)
     in toJSON [toJSON eventTime, toJSON typ, toJSON dat]

instance FromJSON Event where
  parseJSON v = do
    l <- parseJSON v
    case l of
      [time, typ, dat] -> do
        eventTime <- parseJSON time
        typeStr <- parseJSON typ
        eventData <- case typeStr of
          "i" -> EventInput <$> parseJSON dat
          "o" -> EventOutput <$> parseJSON dat
          _ -> fail $ "Unknown event type: " <> typeStr
        pure Event {..}
      _ -> fail $ "Expected a list with three elements, but got: " <> show l

interleaveEvents :: UTCTime -> [(UTCTime, Text)] -> [(UTCTime, ByteString)] -> [Event]
interleaveEvents start inputs outputs = go (sortOn fst inputs) (sortOn fst outputs)
  where
    go [] [] = []
    go is [] = map (uncurry makeInput) is
    go [] os = map (uncurry makeOutput) os
    go iss@((it, i) : is) oss@((ot, o) : os) =
      if it <= ot
        then makeInput it i : go is oss
        else makeOutput ot o : go iss os
    makeTime :: UTCTime -> Double
    makeTime t =
      let d = diffUTCTime t start
       in realToFrac d
    makeInput :: UTCTime -> Text -> Event
    makeInput t d = Event {eventTime = makeTime t, eventData = EventInput d}
    makeOutput :: UTCTime -> ByteString -> Event
    makeOutput t d = Event {eventTime = makeTime t, eventData = EventOutput $ TE.decodeUtf8With TE.lenientDecode d}

eventSpeedUp :: Double -> Event -> Event
eventSpeedUp s e = e {eventTime = eventTime e * s}
