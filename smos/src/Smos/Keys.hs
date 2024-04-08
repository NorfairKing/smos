{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Keys
  ( KeyPress (..),
    Modifier (..),
    Key (..),
    MatcherConfig (..),
    renderKeyPress,
    renderKey,
    renderModifier,
    renderMatcherConfig,
    P,
    keyP,
    modifierP,
    keyPressP,
    matcherConfigP,
  )
where

import Autodocodec
import Control.Arrow (left)
import Data.Aeson as JSON hiding (Key)
import Data.Containers.ListUtils
import Data.Either
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Void
import GHC.Generics (Generic)
import Graphics.Vty.Input.Events as Vty
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

instance Validity Key where
  validate k =
    mconcat
      [ genericValidate k,
        case k of
          KFun i -> declare "The function key index is positive" $ i >= 0
          KChar c -> validateCharNotUtf16SurrogateCodePoint c
          _ -> valid
      ]

instance HasCodec Key where
  codec = bimapCodec (left errorBundlePretty . parse (keyP <* eof) "yaml text") renderKey codec

instance FromJSON Key where
  parseJSON = parseJSONViaCodec

instance ToJSON Key where
  toJSON = toJSONViaCodec
  toEncoding = toEncodingViaCodec

instance Validity Modifier where
  validate = trivialValidation

instance HasCodec Modifier where
  codec = bimapCodec (left errorBundlePretty . parse (modifierP <* eof) "yaml text") renderModifier codec

instance FromJSON Modifier where
  parseJSON = parseJSONViaCodec

instance ToJSON Modifier where
  toJSON = toJSONViaCodec
  toEncoding = toEncodingViaCodec

data KeyPress = KeyPress
  { keyPressKey :: !Key,
    keyPressMods :: ![Modifier]
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec KeyPress)

instance Validity KeyPress where
  validate kp@(KeyPress _ mods) =
    mconcat [genericValidate kp, declare "Each of the mods appears at most once" $ nubOrd mods == mods]

instance HasCodec KeyPress where
  codec = bimapCodec (left errorBundlePretty . parse (keyPressP <* eof) "yaml text") renderKeyPress codec

type P = Parsec Void Text

renderKey :: Key -> Text
renderKey (KChar '\t') = "<tab>"
renderKey (KChar ' ') = "<space>"
renderKey (KFun i) = "<F" <> T.pack (show i) <> ">"
renderKey (KChar c) = T.singleton c
renderKey k = T.pack $ go $ show k
  where
    -- Because these constructors all start with 'K'

    go [] = []
    go ('K' : s) = "<" <> s <> ">"
    go s = "<" <> s <> ">"

keyP :: P Key
keyP =
  choice'
    [ string' "<Tab>" $> KChar '\t',
      string' "<Space>" $> KChar ' ',
      string' "<UpRight>" $> KUpRight,
      string' "<UpLeft>" $> KUpLeft,
      string' "<Up>" $> KUp,
      string' "<Right>" $> KRight,
      string' "<PrtScr>" $> KPrtScr,
      string' "<Pause>" $> KPause,
      string' "<PageUp>" $> KPageUp,
      string' "<PageDown>" $> KPageDown,
      string' "<Menu>" $> KMenu,
      string' "<Left>" $> KLeft,
      string' "<Ins>" $> KIns,
      string' "<Home>" $> KHome,
      string' "<Esc>" $> KEsc,
      string' "<Enter>" $> KEnter,
      string' "<End>" $> KEnd,
      string' "<DownRight>" $> KDownRight,
      string' "<DownLeft>" $> KDownLeft,
      string' "<Down>" $> KDown,
      string' "<Del>" $> KDel,
      string' "<Center>" $> KCenter,
      string' "<Begin>" $> KBegin,
      string' "<BackTab>" $> KBackTab,
      string' "<BS>" $> KBS,
      do
        void $ string' "<F"
        i <- decimal
        void $ string' ">"
        pure $ KFun i,
      KChar <$> satisfy (const True)
    ]

renderModifier :: Modifier -> Text
renderModifier = \case
  MShift -> "Shift"
  MCtrl -> "Ctrl"
  MMeta -> "Meta"
  MAlt -> "Alt"

modifierP :: P Modifier
modifierP =
  choice
    [ string' "Shift" $> MShift,
      string' "Ctrl" $> MCtrl,
      string' "Meta" $> MMeta,
      string' "Alt" $> MAlt,
      string' "S" $> MShift,
      string' "C" $> MCtrl,
      string' "M" $> MMeta,
      string' "A" $> MAlt
    ]

renderKeyPress :: KeyPress -> Text
renderKeyPress (KeyPress key mods) =
  case mods of
    [] -> renderKey key
    _ -> T.intercalate "+" $ map renderModifier mods ++ [renderKey key]

keyPressP :: P KeyPress
keyPressP = do
  mods <-
    many $
      try $
        do
          m <- modifierP
          void $ string' "+" <|> string "-"
          pure m
  key <- keyP
  pure $ KeyPress key mods

data MatcherConfig
  = MatchConfKeyPress !KeyPress
  | MatchConfAnyChar
  | MatchConfCatchAll -- Rename to 'Any'
  | MatchConfCombination !KeyPress !MatcherConfig
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec MatcherConfig)

instance Validity MatcherConfig

instance HasCodec MatcherConfig where
  codec = bimapCodec (left errorBundlePretty . parse (matcherConfigP <* eof) "yaml text") renderMatcherConfig codec

renderMatcherConfig :: MatcherConfig -> Text
renderMatcherConfig mc =
  case mc of
    MatchConfAnyChar -> "<char>"
    MatchConfCatchAll -> "<any>"
    MatchConfKeyPress kp -> renderKeyPress kp
    MatchConfCombination kp rest ->
      let mkp' =
            case rest of
              MatchConfCombination kp' _ -> Just kp'
              MatchConfKeyPress kp' -> Just kp'
              _ -> Nothing
       in T.concat
            [ renderKeyPress kp,
              case mkp' of
                Nothing -> " "
                Just kp' ->
                  case keyPressMods kp' of
                    [] -> ""
                    _ -> " ",
              renderMatcherConfig rest
            ]

matcherConfigP :: P MatcherConfig
matcherConfigP = choice' [charP, anyP, multipleMatchersP]

charP :: P MatcherConfig
charP = string' "<char>" $> MatchConfAnyChar

anyP :: P MatcherConfig
anyP = string' "<any>" $> MatchConfCatchAll

multipleMatchersP :: P MatcherConfig
multipleMatchersP = go
  where
    go = do
      list <-
        sepBy1
          ((Left <$> (try charP <|> try anyP)) <|> (Right <$> keyPressP))
          (void $ optional $ string' " ")
      case reverse list of
        [] -> pure MatchConfCatchAll
        (l : rest) ->
          if any isLeft rest
            then fail "<char> or <any> not allowed in any position other than the last"
            else do
              let rest' = rights rest
              let l' =
                    case l of
                      Left r -> r
                      Right kp -> MatchConfKeyPress kp
              pure $ foldl (flip MatchConfCombination) l' rest'

choice' :: [P a] -> P a
choice' [] = empty
choice' [a] = a
choice' (a : as) = try a <|> choice' as
