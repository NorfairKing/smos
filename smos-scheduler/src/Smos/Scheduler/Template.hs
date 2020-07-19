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

instance Validity Template where
  validate t = mconcat [genericValidate t, declare "The pieces are normalised" $ normaliseTemplate t == t]

normaliseTemplate :: Template -> Template
normaliseTemplate = Template . go . templatePieces
  where
    go = \case
      [] -> []
      [x] -> [x]
      (TLit t1 : TLit t2 : rest) -> TLit (t1 <> t2) : go (TLit t2 : rest)
      (p : rest) -> p : go rest

data TemplatePiece
  = -- | "Literal text", translates to "Literal text"
    TLit Text
  | -- | A time formatting string
    --
    -- All leading and trailing whitespace is skipped
    --
    -- Example: "[ %F ]", translates to "2020-06-27" on "2020-06-27"
    TTime Text
  | -- |  A relative time formatting string
    --
    -- All leading and trailing whitespace is skipped
    --
    -- "[ %F | tomorrow ]", translates to "2020-06-28" on "2020-06-27"
    TRelTime Text Text
  deriving (Show, Eq, Generic)

instance Validity TemplatePiece where
  validate tp =
    mconcat
      [ genericValidate tp,
        case tp of
          TLit t -> declare "The piece is nonempty" $ not $ T.null t
          TTime t -> declare "The piece is stripped" $ T.strip t == t
          TRelTime t1 t2 ->
            mconcat
              [ declare "The first piece is stripped" $ T.strip t1 == t1,
                declare "The second piece is stripped" $ T.strip t2 == t2
              ]
      ]

renderTimeTemplate :: Template -> Text
renderTimeTemplate = T.concat . map renderTimeTemplatePiece . templatePieces

renderTimeTemplatePiece :: TemplatePiece -> Text
renderTimeTemplatePiece = \case
  TLit t -> t
  TTime t -> "[ " <> t <> " ]"
  TRelTime t1 t2 -> "[ " <> t1 <> " | " <> t2 <> " ]"

parseTimeTemplate :: Text -> Either String Template
parseTimeTemplate = fmap Template . go . T.unpack
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
        let h = goRelVar l
        t <-
          if null cs'
            then Left "Unmatched [" -- the string does not contain ']'
            else go (drop 1 cs') -- The first character is ']'
        pure $ h : t
    goRelVar :: String -> TemplatePiece
    goRelVar s =
      let tt = T.strip . T.pack
       in case break (== '|') s of
            ([], []) -> TTime ""
            (_, []) -> TTime (tt s)
            (b, e) -> TRelTime (tt b) (tt (drop 1 e))
