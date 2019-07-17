{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Keys
  ( KeyPress(..)
  , Modifier(..)
  , Key(..)
  , renderKeypress
  , renderKey
  , renderMod
  , P
  , keyP
  , modifierP
  , keyPressP
  ) where

import Import

import Data.Aeson as JSON
import Data.Functor
import qualified Data.Text as T
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Graphics.Vty.Input.Events as Vty

data KeyPress =
  KeyPress Key [Modifier]
  deriving (Show, Eq, Ord, Generic)

instance Validity Key where
  validate = trivialValidation

instance Validity Modifier where
  validate = trivialValidation

instance Validity KeyPress where
  validate = trivialValidation

instance ToJSON KeyPress where
  toJSON kp = JSON.String $ renderKeypress kp

instance FromJSON KeyPress where
  parseJSON = undefined

instance ToJSON Key

instance FromJSON Key

instance ToJSON Modifier

instance FromJSON Modifier

renderKeypress :: KeyPress -> Text
renderKeypress (KeyPress key mods) =
  case mods of
    [] -> renderKey key
    _ -> T.intercalate "-" $ map renderMod mods ++ [renderKey key]

renderKey :: Key -> Text
renderKey (KChar '\t') = "<tab>"
renderKey (KChar ' ') = "<space>"
renderKey (KChar c) = T.singleton c
renderKey KBackTab = "S-<tab>"
renderKey (KFun i) = "F" <> T.pack (show i)
renderKey k = T.pack $ go $ show k
    -- Because these constructors all start with 'K'
  where
    go [] = []
    go ('K':s) = s
    go s = s

renderMod :: Modifier -> Text
renderMod MShift = "S"
renderMod MCtrl = "C"
renderMod MMeta = "M"
renderMod MAlt = "A"

type P = Parsec Void Text

keyP :: P Modifier
keyP = undefined

modifierP :: P Modifier
modifierP =
  choice [string' "S" $> MShift, string' "C" $> MCtrl, string' "M" $> MMeta, string' "A" $> MAlt]

keyPressP :: P Modifier
keyPressP = undefined
