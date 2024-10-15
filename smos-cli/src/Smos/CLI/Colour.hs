{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Smos.CLI.Colour where

import Autodocodec
import Data.Maybe
import OptEnvConf
import Text.Colour
import Text.Colour.Code
import Text.Colour.Layout

colourConfigurationTopLevelObjectCodec :: JSONObjectCodec (Maybe ColourConfiguration)
colourConfigurationTopLevelObjectCodec =
  optionalFieldOrNull "colour" "Colour configuration"

data ColourConfiguration = ColourConfiguration
  { -- | How to background-colour tables
    --
    -- The first maybe is for whether this is defined in the configuration file.
    -- The second maybe is for whether any background colour should be used.
    colourConfigurationBackground :: !(Maybe TableBackgroundConfiguration)
  }

instance HasCodec ColourConfiguration where
  codec = object "ColourConfiguration" objectCodec

instance HasObjectCodec ColourConfiguration where
  objectCodec =
    ColourConfiguration
      <$> optionalFieldOrNull "background" "The table background colours" .= colourConfigurationBackground

data TableBackgroundConfiguration
  = UseTableBackground !TableBackground
  | NoTableBackground

instance HasCodec TableBackgroundConfiguration where
  codec = dimapCodec f g $ eitherCodec nullCodec codec
    where
      f = \case
        Left () -> NoTableBackground
        Right tb -> UseTableBackground tb
      g = \case
        NoTableBackground -> Left ()
        UseTableBackground tb -> Right tb

instance HasCodec TableBackground where
  codec =
    dimapCodec f g $
      eitherCodec
        (codec <?> "A single background colour")
        ( object "Bicolour" $
            (,)
              <$> optionalFieldOrNull "even" "background for even-numbered table-rows (0-indexed)" .= fst
              <*> optionalFieldOrNull "odd" "background for odd-numbered table-rows" .= snd
        )
    where
      f = \case
        Left c -> SingleColour c
        Right (e, o) -> Bicolour e o
      g = \case
        SingleColour c -> Left c
        Bicolour e o -> Right (e, o)

instance HasCodec Colour where
  codec =
    named "Colour" $
      dimapCodec from to $
        eitherCodec colour8Codec $
          eitherCodec colour8BitCodec colour24BitCodec
    where
      from = \case
        Left (i, tc) -> Colour8 i tc
        Right (Left w) -> Colour8Bit w
        Right (Right (r, g, b)) -> Colour24Bit r g b
      to = \case
        Colour8 i tc -> Left (i, tc)
        Colour8Bit w -> Right (Left w)
        Colour24Bit r g b -> Right (Right (r, g, b))
      colour8Codec =
        bimapCodec
          ( \t -> do
              let colourCase :: String -> Either String TerminalColour
                  colourCase = \case
                    "black" -> Right Black
                    "red" -> Right Red
                    "green" -> Right Green
                    "yellow" -> Right Yellow
                    "blue" -> Right Blue
                    "magenta" -> Right Magenta
                    "cyan" -> Right Cyan
                    "white" -> Right White
                    s -> Left $ "Unknown colour: " <> s
              case words t of
                [colourStr] -> (,) Dull <$> colourCase colourStr
                [intensityStr, colourStr] -> do
                  intensity <- case intensityStr of
                    "bright" -> Right Bright
                    "dull" -> Right Dull
                    _ -> Left $ "Unknown colour intensity: " <> intensityStr
                  (,) intensity <$> colourCase colourStr
                _ -> Left "Specify a terminal colour as two words, e. g. 'dull red' or 'bright blue'."
          )
          ( \(intensity, colour8) ->
              mconcat
                [ case intensity of
                    Dull -> ""
                    Bright -> "bright ",
                  case colour8 of
                    Black -> "black"
                    Red -> "red"
                    Green -> "green"
                    Yellow -> "yellow"
                    Blue -> "blue"
                    Magenta -> "magenta"
                    Cyan -> "cyan"
                    White -> "white"
                ]
          )
          codec
      colour8BitCodec =
        codec
          <??> [ "Set this to a number between 0 and 255 that represents the colour that you want from the 8-bit colour schema.",
                 "See this overview on wikipedia for more information:",
                 "https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit"
               ]
      colour24BitCodec =
        object "Colour24Bit" $
          let r (x, _, _) = x
              g (_, x, _) = x
              b (_, _, x) = x
           in (,,)
                <$> requiredField "red" "The red component, [0..255]" .= r
                <*> requiredField "green" "The green component, [0..255]" .= g
                <*> requiredField "blue" "The blue component, [0..255]" .= b

getColourSettings :: Maybe ColourConfiguration -> ColourSettings
getColourSettings mcc =
  ColourSettings
    { colourSettingBackground =
        fromMaybe
          (colourSettingBackground defaultColourSettings)
          (mcc >>= colourConfigurationBackground)
    }

defaultColourSettings :: ColourSettings
defaultColourSettings =
  ColourSettings
    { colourSettingBackground =
        UseTableBackground (Bicolour (Just (Colour8Bit 234)) (Just (Colour8Bit 235)))
    }

data ColourSettings = ColourSettings
  { colourSettingBackground :: !TableBackgroundConfiguration
  }

instance OptEnvConf.HasParser ColourSettings where
  settingsParser = parseColourSettings

{-# ANN parseColourSettings ("NOCOVER" :: String) #-}
parseColourSettings :: OptEnvConf.Parser ColourSettings
parseColourSettings = subAll "colour" $ do
  colourSettingBackground <-
    setting
      [ help "Table background colours",
        conf "background"
      ]
  pure ColourSettings {..}
