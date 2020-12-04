{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Report.Filter where

import Control.Arrow
import Control.DeepSeq
import Control.Monad
import Cursor.Simple.Forest
import Cursor.Simple.Tree
import Data.Aeson
import Data.Char as Char
import Data.Function
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Lens.Micro
import Path
import Smos.Data
import Smos.Report.Comparison
import Smos.Report.Time hiding (P)
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec.Char
import Text.Read (readMaybe)
import YamlParse.Applicative

data Paren
  = OpenParen
  | ClosedParen
  deriving (Show, Eq, Generic)

instance Validity Paren

instance NFData Paren

renderParen :: Paren -> Char
renderParen =
  \case
    OpenParen -> '('
    ClosedParen -> ')'

data BinOp
  = AndOp
  | OrOp
  deriving (Show, Eq, Generic)

instance Validity BinOp

instance NFData BinOp

renderBinOp :: BinOp -> Text
renderBinOp =
  \case
    AndOp -> "and"
    OrOp -> "or"

newtype Piece = Piece
  { pieceText :: Text
  }
  deriving (Show, Eq, Generic)

instance Validity Piece where
  validate p@(Piece t) =
    mconcat
      [ genericValidate p,
        declare "The piece is not empty" $ not $ T.null t,
        decorate "The characters are restricted" $ decorateList (T.unpack t) validateRestrictedChar
      ]

validateRestrictedChar :: Char -> Validation
validateRestrictedChar c =
  decorate "The character is restricted" $
    mconcat
      -- declare "The character is not a UTF16 surrogate codepoint" $ not $ isUtf16SurrogateCodePoint c
      -- Only need to check one of these.
      [declare "The character is printable" $ Char.isPrint c, validateExtraRestrictedChar c]

validateExtraRestrictedChar :: Char -> Validation
validateExtraRestrictedChar c =
  decorate "The character is restricted" $
    mconcat
      [ declare "The character is not a space" $ not $ Char.isSpace c,
        declare "The character is not an open bracket" $ c /= '(',
        declare "The character is not an close bracket" $ c /= ')',
        declare "The character is not a column" $ c /= ':'
      ]

instance NFData Piece

data Part
  = PartParen Paren
  | PartSpace
  | PartColumn
  | PartPiece Piece
  | PartBinOp BinOp
  deriving (Show, Eq, Generic)

instance Validity Part

instance NFData Part

renderPart :: Part -> Text
renderPart =
  \case
    PartParen p -> T.singleton $ renderParen p
    PartSpace -> " "
    PartColumn -> ":"
    PartPiece p -> pieceText p
    PartBinOp bo -> renderBinOp bo

newtype Parts = Parts
  { unParts :: [Part]
  }
  deriving (Show, Eq, Generic)

instance Validity Parts where
  validate p@(Parts ps) =
    mconcat
      [ genericValidate p,
        declare "There are no two unfitting consequtive pieces" $
          let go [] = True
              go [_] = True
              go (p1 : p2 : rest) =
                let b =
                      case p1 of
                        PartPiece _ ->
                          case p2 of
                            PartPiece _ -> False
                            PartBinOp _ -> False
                            _ -> True
                        _ -> True
                 in b && go (p2 : rest)
           in go ps
      ]

instance NFData Parts

renderParts :: Parts -> Text
renderParts = T.concat . map renderPart . unParts

parseParts :: Text -> Either ParseError Parts
parseParts = parse partsP "parts"

partsP :: TP Parts
partsP = Parts <$> many partP

parsePart :: Text -> Either ParseError Part
parsePart = parse partP "part"

partP :: TP Part
partP =
  choice
    [ void (char ':') >> pure PartColumn,
      void (char '(') >> pure (PartParen OpenParen),
      void (char ')') >> pure (PartParen ClosedParen),
      void (char ' ') >> pure PartSpace,
      try $ void (string "and") >> pure (PartBinOp AndOp),
      try $ void (string "or") >> pure (PartBinOp OrOp),
      PartPiece . Piece . T.pack <$> many1 (satisfy (validationIsValid . validateRestrictedChar))
    ]

type TP = Parsec Text ()

type PP = Parsec [Part] ()

part :: Monad m => (Part -> Bool) -> ParsecT [Part] u m Part
part func = tokenPrim showPart nextPos testPart
  where
    showPart = show
    testPart x =
      if func x
        then Just x
        else Nothing
    nextPos pos x _ = updatePosPart pos x
      where
        updatePosPart :: SourcePos -> Part -> SourcePos
        updatePosPart sp p =
          let l =
                case p of
                  PartParen _ -> 1
                  PartSpace -> 1
                  PartColumn -> 1
                  PartPiece (Piece t) -> T.length t
                  PartBinOp bo ->
                    case bo of
                      AndOp -> 3
                      OrOp -> 2
           in incSourceColumn sp l

pieceP :: PP Piece
pieceP = do
  p <- part (const True)
  case p of
    PartPiece t -> pure t
    _ -> fail "expected piece"

data KeyWord
  = KeyWordFile
  | KeyWordTime
  | KeyWordHeader
  | KeyWordTodoState
  | KeyWordProperties
  | KeyWordTags
  | KeyWordCursor
  | KeyWordLevel
  | KeyWordParent
  | KeyWordAncestor
  | KeyWordChild
  | KeyWordLegacy
  | KeyWordAny
  | KeyWordAll
  | KeyWordHas
  | KeyWordVal
  | KeyWordFst
  | KeyWordSnd
  | KeyWordMaybe
  | KeyWordSub
  | KeyWordOrd
  | KeyWordNot
  deriving (Show, Eq, Generic)

instance Validity KeyWord

instance NFData KeyWord

parseKeyword :: Text -> Maybe KeyWord
parseKeyword =
  \case
    "file" -> Just KeyWordFile
    "time" -> Just KeyWordTime
    "header" -> Just KeyWordHeader
    "state" -> Just KeyWordTodoState
    "property" -> Just KeyWordProperties
    "properties" -> Just KeyWordProperties
    "tag" -> Just KeyWordTags
    "tags" -> Just KeyWordTags
    "cursor" -> Just KeyWordCursor
    "level" -> Just KeyWordLevel
    "parent" -> Just KeyWordParent
    "ancestor" -> Just KeyWordAncestor
    "child" -> Just KeyWordChild
    "legacy" -> Just KeyWordLegacy
    "any" -> Just KeyWordAny
    "all" -> Just KeyWordAll
    "has" -> Just KeyWordHas
    "val" -> Just KeyWordVal
    "fst" -> Just KeyWordFst
    "snd" -> Just KeyWordSnd
    "maybe" -> Just KeyWordMaybe
    "sub" -> Just KeyWordSub
    "ord" -> Just KeyWordOrd
    "not" -> Just KeyWordNot
    _ -> Nothing

parseKeywordPiece :: Piece -> Maybe KeyWord
parseKeywordPiece = parseKeyword . pieceText

renderKeyWord :: KeyWord -> Text
renderKeyWord =
  \case
    KeyWordFile -> "file"
    KeyWordTime -> "time"
    KeyWordHeader -> "header"
    KeyWordTodoState -> "state"
    KeyWordProperties -> "properties"
    KeyWordTags -> "tags"
    KeyWordCursor -> "cursor"
    KeyWordLevel -> "level"
    KeyWordParent -> "parent"
    KeyWordAncestor -> "ancestor"
    KeyWordChild -> "child"
    KeyWordLegacy -> "legacy"
    KeyWordAny -> "any"
    KeyWordAll -> "all"
    KeyWordHas -> "has"
    KeyWordVal -> "val"
    KeyWordFst -> "fst"
    KeyWordSnd -> "snd"
    KeyWordMaybe -> "maybe"
    KeyWordSub -> "sub"
    KeyWordOrd -> "ord"
    KeyWordNot -> "not"

anyKeyWordP :: PP KeyWord
anyKeyWordP = do
  p <- pieceP
  case parseKeywordPiece p of
    Just kw -> pure kw
    Nothing -> fail $ "not a keyword: " <> T.unpack (pieceText p)

keyWordP :: KeyWord -> PP ()
keyWordP kw = do
  p <- pieceP
  case parseKeywordPiece p of
    Just kw'
      | kw' == kw' -> pure ()
    _ ->
      fail $
        "expected keyword: " <> T.unpack (renderKeyWord kw) <> " got: "
          <> T.unpack (pieceText p)

data Ast
  = AstBinOp Ast BinOp Ast
  | AstUnOp Piece Ast
  | AstPiece Piece
  deriving (Show, Eq, Generic)

instance Validity Ast

instance NFData Ast

renderAst :: Ast -> Parts
renderAst = Parts . go
  where
    go =
      \case
        AstPiece p -> [PartPiece p]
        AstUnOp p a -> PartPiece p : PartColumn : go a
        AstBinOp a1 bo a2 ->
          concat
            [ [PartParen OpenParen],
              go a1,
              [PartSpace, PartBinOp bo, PartSpace],
              go a2,
              [PartParen ClosedParen]
            ]

parseAst :: Parts -> Either ParseError Ast
parseAst = parse astP "ast" . unParts

astP :: PP Ast
astP = choice [try astBinOpP, try astUnOpP, AstPiece <$> pieceP]

astBinOpP :: PP Ast
astBinOpP = do
  parenP OpenParen
  void $ optional spaceP
  a1 <- astP
  void $ optional spaceP
  bo <- binOpP
  void $ optional spaceP
  a2 <- astP
  void $ optional spaceP
  parenP ClosedParen
  pure $ AstBinOp a1 bo a2

binOpP :: PP BinOp
binOpP = do
  PartBinOp bo <-
    part
      ( \case
          PartBinOp _ -> True
          _ -> False
      )
  pure bo

astUnOpP :: PP Ast
astUnOpP = do
  p <- pieceP
  columnP
  a <- astP
  pure $ AstUnOp p a

parenP :: Paren -> PP ()
parenP p =
  void $
    part
      ( \case
          PartParen p' -> p' == p
          _ -> False
      )

columnP :: PP ()
columnP =
  void $
    part
      ( \case
          PartColumn -> True
          _ -> False
      )

spaceP :: PP ()
spaceP =
  void $
    part
      ( \case
          PartSpace -> True
          _ -> False
      )

type EntryFilterRel = Filter (Path Rel File, ForestCursor Entry)

type ProjectFilter = Filter (Path Rel File)

forestCursorCurrent :: ForestCursor a -> a
forestCursorCurrent fc = fc ^. forestCursorSelectedTreeL . treeCursorCurrentL

forestCursorChildren :: ForestCursor a -> [ForestCursor a]
forestCursorChildren a =
  mapMaybe
    (\i -> a & forestCursorSelectBelowAtPos i)
    ( let CNode _ cf = rebuildTreeCursor $ a ^. forestCursorSelectedTreeL
       in [0 .. length (rebuildCForest cf) - 1]
    )

forestCursorLevel :: ForestCursor a -> Word
forestCursorLevel fc = go' $ fc ^. forestCursorSelectedTreeL
  where
    go' tc =
      case tc ^. treeCursorAboveL of
        Nothing -> 0
        Just tc' -> 1 + goA' tc'
    goA' ta =
      case treeAboveAbove ta of
        Nothing -> 0
        Just ta' -> 1 + goA' ta'

data Filter a where
  FilterFile :: Path Rel File -> Filter (Path Rel File)
  -- Parsing filters
  FilterPropertyTime :: Filter (Maybe Time) -> Filter PropertyValue
  -- Entry mapping filters
  FilterEntryHeader :: Filter Header -> Filter Entry
  FilterEntryTodoState :: Filter (Maybe TodoState) -> Filter Entry
  FilterEntryProperties :: Filter (Map PropertyName PropertyValue) -> Filter Entry
  FilterEntryTags :: Filter (Set Tag) -> Filter Entry
  -- Cursor-related filters
  FilterWithinCursor :: Filter a -> Filter (ForestCursor a)
  FilterLevel :: Word -> Filter (ForestCursor a)
  FilterParent :: Filter (ForestCursor a) -> Filter (ForestCursor a)
  FilterAncestor :: Filter (ForestCursor a) -> Filter (ForestCursor a)
  FilterChild :: Filter (ForestCursor a) -> Filter (ForestCursor a)
  FilterLegacy :: Filter (ForestCursor a) -> Filter (ForestCursor a)
  -- List filters
  FilterAny :: Filter a -> Filter (Set a)
  FilterAll :: Filter a -> Filter (Set a)
  -- Map filters
  FilterMapHas :: (Validity k, NFData k, Show k, Ord k, FilterArgument k) => k -> Filter (Map k v)
  FilterMapVal ::
    (Validity k, NFData k, Show k, Ord k, FilterArgument k) =>
    k ->
    Filter (Maybe v) ->
    Filter (Map k v)
  -- Tuple filters
  FilterFst :: Filter a -> Filter (a, b)
  FilterSnd :: Filter b -> Filter (a, b)
  -- Maybe filters
  FilterMaybe :: Bool -> Filter a -> Filter (Maybe a)
  -- Comparison filters
  FilterSub ::
    (Validity a, NFData a, Show a, Ord a, FilterArgument a, FilterSubString a) =>
    a ->
    Filter a
  FilterOrd ::
    (Validity a, NFData a, Show a, Ord a, FilterArgument a, FilterOrd a) =>
    Comparison ->
    a ->
    Filter a
  -- Boolean filters
  FilterNot :: Filter a -> Filter a
  FilterAnd :: Filter a -> Filter a -> Filter a
  FilterOr :: Filter a -> Filter a -> Filter a

instance NFData (Filter a) where
  rnf =
    \case
      FilterFile f -> rnf f
      FilterPropertyTime f' -> rnf f'
      FilterEntryHeader f' -> rnf f'
      FilterEntryTodoState f' -> rnf f'
      FilterEntryProperties f' -> rnf f'
      FilterEntryTags f' -> rnf f'
      FilterWithinCursor f' -> rnf f'
      FilterLevel w -> rnf w
      FilterParent f' -> rnf f'
      FilterAncestor f' -> rnf f'
      FilterChild f' -> rnf f'
      FilterLegacy f' -> rnf f'
      FilterAny f' -> rnf f'
      FilterAll f' -> rnf f'
      FilterMapHas k -> rnf k
      FilterMapVal k mf -> deepseq k $ rnf mf
      FilterFst f' -> rnf f'
      FilterSnd f' -> rnf f'
      FilterMaybe b f' -> seq b $ rnf f'
      FilterSub a -> rnf a
      FilterOrd o a -> seq o $ rnf a
      FilterNot f' -> rnf f'
      FilterAnd f1 f2 -> deepseq f1 $ rnf f2
      FilterOr f1 f2 -> deepseq f1 $ rnf f2

class FilterSubString a where
  filterSubString :: a -> a -> Bool

instance FilterSubString Header where
  filterSubString = T.isInfixOf `on` renderArgument

instance FilterSubString TodoState where
  filterSubString = T.isInfixOf `on` renderArgument

instance FilterSubString PropertyValue where
  filterSubString = T.isInfixOf `on` renderArgument

instance FilterSubString Tag where
  filterSubString = T.isInfixOf `on` renderArgument

class FilterOrd a

instance FilterOrd Word

instance FilterOrd Time

instance FilterOrd Timestamp

class FilterArgument a where
  renderArgument :: a -> Text
  parseArgument :: Text -> Either String a

parseArgumentPiece :: FilterArgument a => Piece -> Either String a
parseArgumentPiece = parseArgument . pieceText

instance FilterArgument Bool where
  renderArgument = T.pack . show
  parseArgument =
    \case
      "true" -> pure True
      "false" -> pure False
      "True" -> pure True
      "False" -> pure False
      _ -> Left "Invalid bool"

instance FilterArgument Word where
  renderArgument = T.pack . show
  parseArgument = maybe (Left "Invalid word") Right . readMaybe . T.unpack

instance FilterArgument Comparison where
  renderArgument = renderComparison
  parseArgument = maybe (Left "Invalid comparison") Right . parseComparison

instance FilterArgument (Path Rel File) where
  renderArgument = T.pack . fromRelFile
  parseArgument = left show . parseRelFile . T.unpack

instance FilterArgument Header where
  renderArgument = headerText
  parseArgument = parseHeader

instance FilterArgument TodoState where
  renderArgument = todoStateText
  parseArgument = parseTodoState

instance FilterArgument PropertyName where
  renderArgument = propertyNameText
  parseArgument = parsePropertyName

instance FilterArgument PropertyValue where
  renderArgument = propertyValueText
  parseArgument = parsePropertyValue

instance FilterArgument TimestampName where
  renderArgument = timestampNameText
  parseArgument = parseTimestampName

instance FilterArgument Timestamp where
  renderArgument = timestampText
  parseArgument = maybe (Left "Invalid timestamp") Right . parseTimestampText

instance FilterArgument Tag where
  renderArgument = tagText
  parseArgument = parseTag

instance FilterArgument Time where
  renderArgument = renderTime
  parseArgument = parseTime

instance Validity (Filter a) where
  validate f =
    let validateArgumentStr s = decorateList s validateRestrictedChar
        validateArgument a =
          mconcat
            [ validate a,
              declare "The argument is not empty" $ not $ T.null $ renderArgument a,
              decorate "The characters are extra restricted" $
                decorateList (T.unpack $ renderArgument a) validateExtraRestrictedChar
            ]
     in case f of
          FilterFile s ->
            mconcat
              [ validate s,
                declare "The argument is not empty" $ not $ null $ fromRelFile s,
                decorate "The characters are restricted" $ validateArgumentStr $ fromRelFile s
              ]
          FilterPropertyTime f' -> validate f'
          FilterEntryHeader f' -> validate f'
          FilterEntryTodoState f' -> validate f'
          FilterEntryProperties f' -> validate f'
          FilterEntryTags f' -> validate f'
          FilterWithinCursor f' -> validate f'
          FilterLevel w -> validate w
          FilterParent f' -> validate f'
          FilterAncestor f' -> validate f'
          FilterChild f' -> validate f'
          FilterLegacy f' -> validate f'
          FilterAny f' -> validate f'
          FilterAll f' -> validate f'
          FilterMapHas a -> mconcat [validate a, validateArgument a]
          FilterMapVal a f' -> mconcat [validate a, validateArgument a, validate f']
          FilterFst f' -> validate f'
          FilterSnd f' -> validate f'
          FilterMaybe b f' -> mconcat [validate b, validate f']
          FilterSub a -> mconcat [validateArgument a]
          FilterOrd o a -> mconcat [validate o, validateArgument a]
          FilterNot f' -> validate f'
          FilterAnd f1 f2 -> mconcat [validate f1, validate f2]
          FilterOr f1 f2 -> mconcat [validate f1, validate f2]

deriving instance Show (Filter a)

deriving instance Eq (Filter a)

deriving instance Ord (Filter a)

instance FromJSON (Filter (Path Rel File, ForestCursor Entry)) where
  parseJSON = viaYamlSchema

instance YamlSchema (Filter (Path Rel File, ForestCursor Entry)) where
  yamlSchema = eitherParser (left (T.unpack . prettyFilterParseError) . parseEntryFilterRel) yamlSchema

instance ToJSON (Filter a) where
  toJSON = toJSON . renderFilter

foldFilterAnd :: NonEmpty (Filter a) -> Filter a
foldFilterAnd = foldl1 FilterAnd

filterPredicate :: Filter a -> a -> Bool
filterPredicate = go
  where
    go :: forall a. Filter a -> a -> Bool
    go f a =
      let goF f' = go f' a
          goProj :: forall b. (a -> b) -> Filter b -> Bool
          goProj func f' = go f' $ func a
       in case f of
            FilterFile rp -> fromRelFile rp `isInfixOf` fromRelFile a
            -- Parsing filters
            FilterPropertyTime f' -> goProj (time . propertyValueText) f'
            -- Entry mapping filters
            FilterEntryHeader f' -> goProj entryHeader f'
            FilterEntryTodoState f' -> goProj entryState f'
            FilterEntryProperties f' -> goProj entryProperties f'
            FilterEntryTags f' -> goProj entryTags f'
            -- Cursor-related filters
            FilterWithinCursor f' -> go f' (forestCursorCurrent a)
            FilterLevel l -> l == forestCursorLevel a
            FilterAncestor f' ->
              maybe False (\fc_ -> go f' fc_ || go f fc_) (forestCursorSelectAbove a) || go f' a
            FilterLegacy f' ->
              any (\fc_ -> go f' fc_ || go f fc_) (forestCursorChildren a) || go f' a
            FilterParent f' -> maybe False (go f') (forestCursorSelectAbove a)
            FilterChild f' -> any (go f') (forestCursorChildren a)
            -- List filters
            FilterAny f' -> any (go f') a
            FilterAll f' -> all (go f') a
            -- Map filters
            FilterMapHas k -> M.member k a
            FilterMapVal k f' -> goProj (M.lookup k) f'
            -- Tuple filters
            FilterFst f' -> goProj fst f'
            FilterSnd f' -> goProj snd f'
            -- Maybe filters
            FilterMaybe b f' -> maybe b (go f') a
            -- Comparison filters
            FilterSub t -> t `filterSubString` a
            FilterOrd o a' -> comparisonFunc o a a'
            -- Boolean filters
            FilterNot f' -> not $ goF f'
            FilterAnd f1 f2 -> goF f1 && goF f2
            FilterOr f1 f2 -> goF f1 || goF f2

renderFilter :: Filter a -> Text
renderFilter = renderParts . renderAst . renderFilterAst

renderFilterAst :: Filter a -> Ast
renderFilterAst = go
  where
    go :: Filter a -> Ast
    go =
      let pa :: FilterArgument a => a -> Ast
          pa a = AstPiece $ Piece $ renderArgument a
          paa :: FilterArgument a => a -> Ast -> Ast
          paa a = AstUnOp (Piece $ renderArgument a)
          pkw :: KeyWord -> Ast -> Ast
          pkw kw = AstUnOp (Piece $ renderKeyWord kw)
          kwa :: KeyWord -> Filter a -> Ast
          kwa kw f' = pkw kw $ go f'
          kwp :: FilterArgument a => KeyWord -> a -> Ast
          kwp kw a = pkw kw $ pa a
          kwb :: Filter a -> BinOp -> Filter a -> Ast
          kwb f1 bo f2 = AstBinOp (go f1) bo (go f2)
       in \case
            FilterFile rp -> kwp KeyWordFile rp
            FilterPropertyTime f' -> kwa KeyWordTime f'
            FilterEntryHeader f' -> kwa KeyWordHeader f'
            FilterEntryTodoState f' -> kwa KeyWordTodoState f'
            FilterEntryProperties f' -> kwa KeyWordProperties f'
            FilterEntryTags f' -> kwa KeyWordTags f'
            FilterWithinCursor f' -> kwa KeyWordCursor f'
            FilterLevel l -> kwp KeyWordLevel l
            FilterAncestor f' -> kwa KeyWordAncestor f'
            FilterLegacy f' -> kwa KeyWordLegacy f'
            FilterParent f' -> kwa KeyWordParent f'
            FilterChild f' -> kwa KeyWordChild f'
            FilterAny f' -> kwa KeyWordAny f'
            FilterAll f' -> kwa KeyWordAll f'
            FilterMapHas k -> pkw KeyWordHas $ pa k
            FilterMapVal k f' -> pkw KeyWordVal $ paa k $ go f'
            FilterFst f' -> kwa KeyWordFst f'
            FilterSnd f' -> kwa KeyWordSnd f'
            FilterMaybe b f' -> pkw KeyWordMaybe $ paa b $ go f'
            FilterSub t -> kwp KeyWordSub t
            FilterOrd o a -> pkw KeyWordOrd $ paa o $ pa a
            FilterNot f' -> kwa KeyWordNot f'
            FilterOr f1 f2 -> kwb f1 OrOp f2
            FilterAnd f1 f2 -> kwb f1 AndOp f2

data FilterParseError
  = TokenisationError ParseError
  | ParsingError ParseError
  | TypeCheckingError FilterTypeError
  deriving (Show, Eq, Generic)

prettyFilterParseError :: FilterParseError -> Text
prettyFilterParseError =
  \case
    TokenisationError pe -> T.pack $ show pe
    ParsingError pe -> T.pack $ show pe
    TypeCheckingError te -> renderFilterTypeError te

parseEntryFilterRel :: Text -> Either FilterParseError EntryFilterRel
parseEntryFilterRel = parseTextFilter parseEntryFilterRelAst

parseProjectFilter :: Text -> Either FilterParseError ProjectFilter
parseProjectFilter = parseTextFilter parseProjectFilterAst

parseTextFilter :: TC a -> Text -> Either FilterParseError a
parseTextFilter tc t = do
  ps <- left TokenisationError $ parseParts t
  ast <- left ParsingError $ parseAst ps
  left TypeCheckingError $ tc ast

data FilterTypeError
  = FTEPieceExpected Ast
  | FTEUnOpExpected Ast
  | FTEKeyWordExpected Piece
  | FTEThisKeyWordExpected [KeyWord] KeyWord -- Expected, actual
  | FTEArgumentExpected Piece String
  | FTENoChoices
  deriving (Show, Eq, Generic)

renderFilterTypeError :: FilterTypeError -> Text
renderFilterTypeError = T.pack . show

type TCE a = Either FilterTypeError a

type TC a = Ast -> TCE a

parseEntryFilterRelAst :: Ast -> Either FilterTypeError EntryFilterRel
parseEntryFilterRelAst = tcTupleFilter tcFilePathFilter (tcForestCursorFilter tcEntryFilter)

parseProjectFilterAst :: Ast -> Either FilterTypeError ProjectFilter
parseProjectFilterAst = tcFilePathFilter

tcPiece :: (Piece -> TCE a) -> TC a
tcPiece func =
  \case
    AstPiece p -> func p
    ast -> Left $ FTEPieceExpected ast

tcArgumentPiece :: FilterArgument a => (a -> TCE b) -> TC b
tcArgumentPiece func =
  tcPiece $ \p ->
    case parseArgumentPiece p of
      Right a -> func a
      Left s -> Left $ FTEArgumentExpected p s

tcUnOp :: (Piece -> TC a) -> TC a
tcUnOp func =
  \case
    AstUnOp p a -> func p a
    ast -> Left $ FTEUnOpExpected ast

tcArgumentOp :: FilterArgument a => (a -> TC b) -> TC b
tcArgumentOp func =
  tcUnOp $ \p a ->
    case parseArgumentPiece p of
      Right arg -> func arg a
      Left err -> Left $ FTEArgumentExpected p err

tcKeyWord :: (KeyWord -> TCE a) -> Piece -> TCE a
tcKeyWord func p =
  case parseKeywordPiece p of
    Just kw -> func kw
    Nothing -> Left $ FTEKeyWordExpected p

tcKeyWordOp :: (KeyWord -> TC a) -> TC a
tcKeyWordOp func = tcUnOp $ \p a -> flip tcKeyWord p $ \kw -> func kw a

tcThisKeyWordOp :: KeyWord -> TC a -> TC a
tcThisKeyWordOp kw' func =
  tcKeyWordOp $ \kw a ->
    if kw == kw'
      then func a
      else Left $ FTEThisKeyWordExpected [kw'] kw

tcWithTopLevelBranches :: TC (Filter a) -> TC (Filter a)
tcWithTopLevelBranches func ast =
  case ast of
    AstBinOp a1 bo a2 ->
      let f = tcWithTopLevelBranches func
       in case bo of
            AndOp -> FilterAnd <$> f a1 <*> f a2
            OrOp -> FilterOr <$> f a1 <*> f a2
    AstUnOp p a ->
      case parseKeywordPiece p of
        Just KeyWordNot -> FilterNot <$> tcWithTopLevelBranches func a
        _ -> func ast
    _ -> func ast

tcFilePathFilter :: TC (Filter (Path Rel File))
tcFilePathFilter = tcWithTopLevelBranches $
  tcThisKeyWordOp KeyWordFile $
    tcPiece $ \p2 ->
      case parseArgumentPiece p2 of
        Right rp -> Right $ FilterFile rp
        Left err -> Left $ FTEArgumentExpected p2 err

tcSub :: (Validity a, NFData a, Show a, Ord a, FilterArgument a, FilterSubString a) => TC (Filter a)
tcSub =
  let subTC = fmap FilterSub . tcArgumentPiece pure
   in tcChoices [tcThisKeyWordOp KeyWordSub subTC, subTC]

tcOrd :: (Validity a, NFData a, Show a, Ord a, FilterArgument a, FilterOrd a) => TC (Filter a)
tcOrd =
  let ordTC =
        tcArgumentOp $ \comparison -> tcArgumentPiece $ \arg -> pure $ FilterOrd comparison arg
   in tcChoices [tcThisKeyWordOp KeyWordOrd ordTC, ordTC]

tcTimeFilter :: TC (Filter Time)
tcTimeFilter = tcWithTopLevelBranches tcOrd

tcTagFilter :: TC (Filter Tag)
tcTagFilter = tcWithTopLevelBranches tcSub

tcHeaderFilter :: TC (Filter Header)
tcHeaderFilter = tcWithTopLevelBranches tcSub

tcTodoStateFilter :: TC (Filter TodoState)
tcTodoStateFilter = tcWithTopLevelBranches tcSub

tcMaybeFilter :: TC (Filter a) -> TC (Filter (Maybe a))
tcMaybeFilter tc =
  tcWithTopLevelBranches $
    tcChoices
      [ tcThisKeyWordOp KeyWordMaybe $ tcArgumentOp $ \b a -> FilterMaybe b <$> tc a,
        fmap (FilterMaybe False) . tc
      ]

tcPropertyValueFilter :: TC (Filter PropertyValue)
tcPropertyValueFilter =
  tcWithTopLevelBranches $
    tcChoices
      [tcThisKeyWordOp KeyWordTime $ fmap FilterPropertyTime . tcMaybeFilter tcTimeFilter, tcSub]

tcMapFilter ::
  (Validity k, NFData k, Show k, Ord k, FilterArgument k) =>
  TC (Filter v) ->
  TC (Filter (Map k v))
tcMapFilter func =
  tcWithTopLevelBranches $
    tcChoices $
      let valTC = tcArgumentOp $ \arg1 -> fmap (FilterMapVal arg1) . tcMaybeFilter func
          hasTC = tcArgumentPiece $ \arg -> pure $ FilterMapHas arg
       in [ tcKeyWordOp $ \kw ->
              case kw of
                KeyWordVal -> valTC
                KeyWordHas -> hasTC
                _ -> const $ Left $ FTEThisKeyWordExpected [KeyWordVal, KeyWordHas] kw,
            valTC,
            hasTC
          ]

tcPropertiesFilter :: TC (Filter (Map PropertyName PropertyValue))
tcPropertiesFilter = tcMapFilter tcPropertyValueFilter

tcSetFilter :: TC (Filter a) -> TC (Filter (Set a))
tcSetFilter func =
  tcWithTopLevelBranches $
    tcChoices $
      let anyTC = fmap FilterAny . func
          allTC = fmap FilterAll . func
       in [ tcKeyWordOp $ \kw ->
              case kw of
                KeyWordAny -> anyTC
                KeyWordAll -> allTC
                _ -> const $ Left $ FTEThisKeyWordExpected [KeyWordAny, KeyWordAll] kw,
            anyTC
          ]

tcTagsFilter :: TC (Filter (Set Tag))
tcTagsFilter = tcSetFilter tcTagFilter

tcEntryFilter :: TC (Filter Entry)
tcEntryFilter =
  tcWithTopLevelBranches $
    tcKeyWordOp $ \kw ->
      case kw of
        KeyWordHeader -> fmap FilterEntryHeader . tcHeaderFilter
        KeyWordTodoState -> fmap FilterEntryTodoState . tcMaybeFilter tcTodoStateFilter
        KeyWordProperties -> fmap FilterEntryProperties . tcPropertiesFilter
        KeyWordTags -> fmap FilterEntryTags . tcTagsFilter
        _ ->
          const $
            Left $
              FTEThisKeyWordExpected [KeyWordHeader, KeyWordTodoState, KeyWordProperties, KeyWordTags] kw

tcForestCursorFilter :: TC (Filter a) -> TC (Filter (ForestCursor a))
tcForestCursorFilter tc =
  tcWithTopLevelBranches $
    tcChoices $
      let withinCursorTC = fmap FilterWithinCursor . tc
       in [ tcKeyWordOp $ \kw ->
              case kw of
                KeyWordParent -> fmap FilterParent . tcForestCursorFilter tc
                KeyWordAncestor -> fmap FilterAncestor . tcForestCursorFilter tc
                KeyWordChild -> fmap FilterChild . tcForestCursorFilter tc
                KeyWordLegacy -> fmap FilterLegacy . tcForestCursorFilter tc
                KeyWordLevel -> fmap FilterLevel . tcArgumentPiece pure
                KeyWordCursor -> withinCursorTC
                _ ->
                  const $
                    Left $
                      FTEThisKeyWordExpected
                        [ KeyWordParent,
                          KeyWordAncestor,
                          KeyWordChild,
                          KeyWordLegacy,
                          KeyWordLevel,
                          KeyWordCursor
                        ]
                        kw,
            withinCursorTC
          ]

tcTupleFilter :: TC (Filter a) -> TC (Filter b) -> TC (Filter (a, b))
tcTupleFilter tc1 tc2 =
  tcWithTopLevelBranches $
    let fstTC = fmap FilterFst . tc1
        sndTC = fmap FilterSnd . tc2
     in tcChoices
          [ tcKeyWordOp $ \kw ->
              case kw of
                KeyWordFst -> fstTC
                KeyWordSnd -> sndTC
                _ -> const $ Left $ FTEThisKeyWordExpected [KeyWordFst, KeyWordSnd] kw,
            fstTC,
            sndTC
          ]

tcChoices :: [TC a] -> TC a
tcChoices [] = const $ Left FTENoChoices
tcChoices [tc] = tc
tcChoices (tc : tcs) =
  \ast ->
    case tc ast of
      Right r -> pure r
      Left _ -> tcChoices tcs ast
