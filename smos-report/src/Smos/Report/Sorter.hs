{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Sorter where

import Autodocodec
import Control.Arrow (left)
import Control.Monad
import Cursor.Simple.Forest
import Cursor.Simple.Tree
import Data.Aeson
import Data.List
import qualified Data.Map as M
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Void
import GHC.Generics (Generic)
import Lens.Micro
import Smos.Data
import Smos.Report.Time hiding (P)
import Text.Megaparsec
import Text.Megaparsec.Char

data Sorter
  = ByFile
  | ByHeader
  | ByState
  | ByTag !Tag
  | ByProperty !PropertyName
  | ByPropertyTime !PropertyName
  | ByTimestamp !TimestampName
  | Reverse !Sorter
  | AndThen !Sorter !Sorter
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec Sorter)

instance Validity Sorter

instance HasCodec Sorter where
  codec = named "Sorter" (bimapCodec parseSorter renderSorter codec <??> sorterDocs)

sorterDocs :: [Text]
sorterDocs = ["A sorter is a string of one of the following forms:", ""] <> sorterFormsDocs

sorterFormsDocs :: [Text]
sorterFormsDocs =
  [ "file",
    "header",
    "state",
    "tag:<tag>",
    "property:<property-name>",
    "property-as-time:<property-name>",
    "timestamp:<timestamp-name>",
    "reverse:<sorter>",
    "(<sorter> then <sorter>)"
  ]

sorterExamples :: [(Text, Sorter)]
sorterExamples =
  [ ("Sort by filename:", ByFile),
    ("Sort by whether the entry has the 'home' tag:", ByTag "home"),
    ("Sort by the string-representation of the value of the 'effort' property:", ByProperty "effort"),
    ("Sort by the time-representation of the value of the 'timewindow' property:", ByPropertyTime "timewindow"),
    ("Sort by the BEGIN timestamp:", ByTimestamp "BEGIN"),
    ("Sort by filename, in reverse:", Reverse ByFile),
    ("Sort by filename, then by the 'effort' property:", ByFile `AndThen` ByProperty "effort")
  ]

sorterSortCursorList ::
  (Ord a) =>
  Sorter ->
  [(a, ForestCursor Entry)] ->
  [(a, ForestCursor Entry)]
sorterSortCursorList s =
  sortBy $ \(rpa, fca) (rpb, fcb) -> sorterOrdering s rpa (cur fca) rpb (cur fcb)
  where
    cur fc = fc ^. forestCursorSelectedTreeL . treeCursorCurrentL

sorterSortList :: (Ord a) => Sorter -> [(a, Entry)] -> [(a, Entry)]
sorterSortList s = sortBy $ \(rpa, ea) (rpb, eb) -> sorterOrdering s rpa ea rpb eb

sorterOrdering :: (Ord a) => Sorter -> a -> Entry -> a -> Entry -> Ordering
sorterOrdering s_ rpa fca_ rpb fcb_ = go s_ fca_ fcb_
  where
    go s ea eb =
      case s of
        ByFile -> compare rpa rpb
        ByHeader -> comparing entryHeader ea eb
        ByState -> comparing entryState ea eb
        ByTag t -> comparing ((t `elem`) . entryTags) ea eb
        ByPropertyTime pn ->
          comparing (\e -> M.lookup pn (entryProperties e) >>= (time . propertyValueText)) ea eb
        ByProperty pn -> comparing (M.lookup pn . entryProperties) ea eb
        ByTimestamp tn -> comparing (fmap timestampLocalTime . M.lookup tn . entryTimestamps) ea eb
        Reverse s' ->
          ( \case
              GT -> LT
              EQ -> EQ
              LT -> GT
          )
            $ go s' ea eb
        AndThen s1 s2 -> go s1 ea eb <> go s2 ea eb

parseSorter :: Text -> Either String Sorter
parseSorter = left errorBundlePretty . parse sorterP ""

type P = Parsec Void Text

sorterP :: P Sorter
sorterP =
  try byFileP
    <|> try byStateP
    <|> try byHeaderP
    <|> try byTagP
    <|> try byPropertyAsTimeP
    <|> try byPropertyP
    <|> try byTimestampP
    <|> try reverseP
    <|> andThenP

byFileP :: P Sorter
byFileP = do
  void $ string' "file"
  pure ByFile

byHeaderP :: P Sorter
byHeaderP = do
  void $ string' "header"
  pure ByHeader

byStateP :: P Sorter
byStateP = do
  void $ string' "state"
  pure ByState

byTagP :: P Sorter
byTagP = do
  void $ string' "tag:"
  ByTag <$> tagP

byPropertyP :: P Sorter
byPropertyP = do
  void $ string' "property:"
  ByProperty <$> propertyNameP

byPropertyAsTimeP :: P Sorter
byPropertyAsTimeP = do
  void $ string' "property-as-time:"
  ByPropertyTime <$> propertyNameP

byTimestampP :: P Sorter
byTimestampP = do
  void $ string' "timestamp:"
  ByTimestamp <$> timestampNameP

reverseP :: P Sorter
reverseP = do
  void $ string' "reverse:"
  Reverse <$> sorterP

andThenP :: P Sorter
andThenP = do
  void $ char '('
  s1 <- sorterP
  void $ string' " then "
  s2 <- sorterP
  void $ char ')'
  pure $ AndThen s1 s2

tagP :: P Tag
tagP = do
  s <- many (satisfy (\c -> validTagChar c && c /= ')'))
  either fail pure $ parseTag $ T.pack s

propertyNameP :: P PropertyName
propertyNameP = do
  s <- many (satisfy (\c -> validPropertyNameChar c && c /= ')'))
  either fail pure $ parsePropertyName $ T.pack s

timestampNameP :: P TimestampName
timestampNameP = do
  s <- many (satisfy (\c -> validTimestampNameChar c && c /= ')'))
  either fail pure $ parseTimestampName $ T.pack s

renderSorter :: Sorter -> Text
renderSorter f =
  case f of
    ByFile -> "file"
    ByHeader -> "header"
    ByState -> "state"
    ByTag t -> "tag:" <> tagText t
    ByProperty pn -> "property:" <> propertyNameText pn
    ByPropertyTime pn -> "property-as-time:" <> propertyNameText pn
    ByTimestamp tn -> "timestamp:" <> timestampNameText tn
    Reverse s' -> "reverse:" <> renderSorter s'
    AndThen s1 s2 -> T.concat ["(", renderSorter s1, " then ", renderSorter s2, ")"]
