{-# LANGUAGE DeriveGeneric #-}

module Smos.Scheduler.Template where

import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)

data Template
  = TLit Text -- "Literal text"
  | TTime Text -- "[%F]", translates to 2020-06-27
  | TRelTime Text -- "[%F|tomorrow]", translates to 2020-06-28
  deriving (Show, Eq, Generic)

instance Validity Template

parseTemplate :: Text -> Either String Template
parseTemplate = undefined

renderTemplate :: Template -> Text
renderTemplate = undefined
