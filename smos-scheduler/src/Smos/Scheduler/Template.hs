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
  = TLit Text -- "Literal text", translates to "Literal text"
  | TTime Text -- "[ %F ]", translates to "2020-06-27"
  | TRelTime Text Text -- "[ %F | tomorrow ]", translates to "2020-06-28"
  deriving (Show, Eq, Generic)

instance Validity TemplatePiece

parseTemplate :: Text -> Either String Template
parseTemplate = undefined

renderTemplate :: Template -> Text
renderTemplate = T.concat . map renderTemplatePiece . templatePieces

renderTemplatePiece :: TemplatePiece -> Text
renderTemplatePiece = \case
  TLit t -> t
  TTime t -> "[ " <> t <> " ]"
  TRelTime t1 t2 -> "[ " <> t1 <> " | " <> t2 <> " ]"
