{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Scheduler.Template where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)

newtype Template = Template {templatePieces :: [TemplatePiece]}
  deriving (Show, Eq, Generic)

instance Validity Template

data TemplatePiece
  = -- | "Literal text", translates to "Literal text"
    TLit Text
  | -- | A time formatting string
    --
    -- All leading and trailing whitespace is skipped
    --
    -- Example: "[ %F ]", translates to "2020-06-27"
    TTime Text
  | -- |  A relative time formatting string
    --
    -- All leading and trailing whitespace is skipped
    --
    -- "[ %F | tomorrow ]", translates to "2020-06-28"
    TRelTime Text Text
  deriving (Show, Eq, Generic)

instance Validity TemplatePiece

renderTemplate :: Template -> Text
renderTemplate = T.concat . map renderTemplatePiece . templatePieces

renderTemplatePiece :: TemplatePiece -> Text
renderTemplatePiece = \case
  TLit t -> t
  TTime t -> "[ " <> t <> " ]"
  TRelTime t1 t2 -> "[ " <> t1 <> " | " <> t2 <> " ]"

parseTemplate :: Text -> Either String Template
parseTemplate = fmap Template . go . T.unpack
  where
    go :: String -> Either String [TemplatePiece]
    go cs = case break (== '[') cs of
      (l, cs') -> do
        let h = [TLit (T.pack l) | not (null l)]
        t <-
          if null cs'
            then pure [] -- the string does not contain '['
            else goVar (drop 1 cs') -- The first character is '['
        pure $ h ++ t
    goVar :: String -> Either String [TemplatePiece]
    goVar cs = case break (== ']') cs of
      (l, cs') -> do
        let h = [TTime (T.strip (T.pack l))]
        t <-
          if null cs'
            then Left "Unmatched [" -- the string does not contain ']'
            else go (drop 1 cs') -- The first character is ']'
        pure $ h ++ t
