{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Smos.Report.Filter where

import GHC.Generics (Generic)

import Data.Aeson
import Data.Char as Char
import Data.Foldable
import Data.Function
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Validity
import Data.Void
import Path
import Text.Read

import Control.Arrow
import Control.Monad

import Lens.Micro

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Cursor.Simple.Forest
import Cursor.Simple.Tree

import Smos.Data

import Smos.Report.Path
import Smos.Report.Time hiding (P)

type EntryFilter = Filter (RootedPath, ForestCursor Entry)

data SmosFileAtPath =
  SmosFileAtPath
    { smosFilePath :: RootedPath
    , smosFileFile :: SmosFile
    }
  deriving (Show, Eq, Generic)

forestCursorCurrent :: ForestCursor a -> a
forestCursorCurrent fc = fc ^. forestCursorSelectedTreeL . treeCursorCurrentL

forestCursorChildren :: ForestCursor a -> [ForestCursor a]
forestCursorChildren a =
  mapMaybe
    (\i -> a & forestCursorSelectBelowAtPos i)
    (let CNode _ cf = rebuildTreeCursor $ a ^. forestCursorSelectedTreeL
      in [0 .. length (rebuildCForest cf) - 1])

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

data Comparison
  = LTC
  | LEC
  | EQC
  | GEC
  | GTC
  deriving (Show, Eq, Ord, Generic)

instance Validity Comparison

comparisonFunc :: Ord a => Comparison -> (a -> a -> Bool)
comparisonFunc c =
  case c of
    LTC -> (<)
    LEC -> (<=)
    EQC -> (==)
    GEC -> (<=)
    GTC -> (<)

renderComparison :: Comparison -> Text
renderComparison c =
  case c of
    LTC -> "lt"
    LEC -> "le"
    EQC -> "eq"
    GEC -> "ge"
    GTC -> "gt"

data Filter a where
  FilterFile :: Path Rel File -> Filter RootedPath
  -- Parsing filters
  FilterPropertyTime :: Filter (Maybe Time) -> Filter PropertyValue
  -- Entry mapping filters
  FilterEntryHeader :: Filter Header -> Filter Entry
  FilterEntryTodoState :: Filter (Maybe TodoState) -> Filter Entry
  FilterEntryProperties :: Filter (Map PropertyName PropertyValue) -> Filter Entry
  FilterEntryTags :: Filter [Tag] -> Filter Entry
  -- Cursor-related filters
  FilterWithinCursor :: Filter a -> Filter (ForestCursor a)
  FilterLevel :: Word -> Filter (ForestCursor a)
  FilterParent :: Filter (ForestCursor a) -> Filter (ForestCursor a)
  FilterAncestor :: Filter (ForestCursor a) -> Filter (ForestCursor a)
  FilterChild :: Filter (ForestCursor a) -> Filter (ForestCursor a)
  FilterLegacy :: Filter (ForestCursor a) -> Filter (ForestCursor a)
  -- List filters
  FilterListHas :: (Validity a, Show a, Ord a, FilterArgument a) => a -> Filter [a]
  FilterAny :: Filter a -> Filter [a]
  FilterAll :: Filter a -> Filter [a]
  -- Map filters
  FilterMapHas :: (Validity k, Show k, Ord k, FilterArgument k) => k -> Filter (Map k v)
  FilterMapVal
    :: (Validity k, Show k, Ord k, FilterArgument k) => k -> Filter (Maybe v) -> Filter (Map k v)
  -- Tuple filters
  FilterFst :: Filter a -> Filter (a, b)
  FilterSnd :: Filter b -> Filter (a, b)
  -- Maybe filters
  FilterMaybe :: Bool -> Filter a -> Filter (Maybe a)
  -- Comparison filters
  FilterSub :: (Validity a, Show a, Ord a, FilterArgument a, FilterSubString a) => a -> Filter a
  FilterOrd
    :: (Validity a, Show a, Ord a, FilterArgument a, FilterOrd a) => Comparison -> a -> Filter a
  -- Boolean filters
  FilterNot :: Filter a -> Filter a
  FilterAnd :: Filter a -> Filter a -> Filter a
  FilterOr :: Filter a -> Filter a -> Filter a

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

instance FilterArgument Word where
  renderArgument = T.pack . show
  parseArgument = maybe (Left "Invalid word") Right . readMaybe . T.unpack

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
    case f of
      FilterFile s ->
        mconcat
          [ validate s
          , declare "The filenames are restricted" $
            all (\c -> not (Char.isSpace c) && c /= ')' && not (isUtf16SurrogateCodePoint c)) $
            fromRelFile s
          ]
      FilterOrd o a ->
        mconcat
          [ validate o
          , validate a
          , declare "The characters are restricted" $
            all (\c -> not (Char.isSpace c) && Char.isPrint c && c /= ')') $
            T.unpack $
            renderArgument a
          , declare "The argument is not empty" $ not $ T.null $ renderArgument a
          ]
      FilterSub a ->
        mconcat
          [ validate a
          , declare "The characters are restricted" $
            all (\c -> not (Char.isSpace c) && Char.isPrint c && c /= ')') $
            T.unpack $
            renderArgument a
          , declare "The argument is not empty" $ not $ T.null $ renderArgument a
          ]
      _ -> trivialValidation f

deriving instance Show (Filter a)

deriving instance Eq (Filter a)

deriving instance Ord (Filter a)

instance FromJSON (Filter (RootedPath, ForestCursor Entry)) where
  parseJSON =
    withText "EntryFilter" $ \t ->
      case parseEntryFilter t of
        Nothing -> fail "could not parse EntryFilter."
        Just f -> pure f

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
            FilterFile rp -> fromRelFile rp `isInfixOf` fromAbsFile (resolveRootedPath a)
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
            FilterListHas a' -> a' `elem` a
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
            FilterSub t -> t `filterSubString` t
            FilterOrd o a' -> comparisonFunc o a a'
            -- Boolean filters
            FilterNot f' -> not $ goF f'
            FilterAnd f1 f2 -> goF f1 && goF f2
            FilterOr f1 f2 -> goF f1 || goF f2

renderFilter :: Filter a -> Text
renderFilter = go
  where
    go :: Filter a -> Text
    go f =
      let p t1 t2 = t1 <> ":" <> t2
          p1 t f' = p t $ go f'
          p2 f1 o f2 = T.concat ["(", renderFilter f1, " ", o, " ", renderFilter f2, ")"]
          bt b =
            if b
              then "true"
              else "false"
       in case f of
            FilterFile rp -> p "file" $ renderArgument rp
            FilterPropertyTime f' -> p1 "time" f'
                -- Entry mapping filters
            FilterEntryHeader f' -> p1 "header" f'
            FilterEntryTodoState f' -> p1 "state" f'
            FilterEntryProperties f' -> p1 "properties" f'
            FilterEntryTags f' -> p1 "tags" f'
                -- Cursor-related filters
            FilterWithinCursor f' -> go f'
            FilterLevel l -> p "level" $ renderArgument l
            FilterAncestor f' -> p1 "ancestor" f'
            FilterLegacy f' -> p1 "legacy" f'
            FilterParent f' -> p1 "parent" f'
            FilterChild f' -> p1 "child" f'
                -- List filters
            FilterListHas k -> p "has" $ renderArgument k
            FilterAny f' -> p1 "any" f'
            FilterAll f' -> p1 "all" f'
                -- Map filters
            FilterMapHas k -> p "has" $ renderArgument k
            FilterMapVal k f' -> p1 (renderArgument k) f'
                -- Tuple filters
            FilterFst f' -> go f'
            FilterSnd f' -> go f'
                -- Maybe filters
            FilterMaybe b f' -> p "maybe" $ p1 (bt b) f'
                -- Comparison filters
            FilterSub t -> renderArgument t
            FilterOrd o a -> p (renderComparison o) (renderArgument a)
                -- Boolean filters
            FilterNot f' -> p1 "not" f'
            FilterOr f1 f2 -> p2 f1 "or" f2
            FilterAnd f1 f2 -> p2 f1 "and" f2

type P = Parsec Void Text

parseEntryFilter :: Text -> Maybe EntryFilter
parseEntryFilter = parseMaybe entryFilterP

entryFilterP :: P EntryFilter
entryFilterP = FilterFst <$> filterRootedPathP

filterRootedPathP :: P (Filter RootedPath)
filterRootedPathP =
  withTopLevelBranchesP $ do
    pieceP "file"
    s <- many (satisfy $ \c -> not (Char.isSpace c) && c /= ')')
    r <- either (fail . show) (pure . FilterFile) $ parseRelFile s
    case prettyValidate r of
      Left err -> fail err
      Right f -> pure f

filterTimeP :: P (Filter Time)
filterTimeP = withTopLevelBranchesP eqAndOrdP

filterTagP :: P (Filter Tag)
filterTagP = withTopLevelBranchesP subP

filterHeaderP :: P (Filter Header)
filterHeaderP = withTopLevelBranchesP subP

filterTodoStateP :: P (Filter TodoState)
filterTodoStateP = withTopLevelBranchesP subP

filterTimestampP :: P (Filter Timestamp)
filterTimestampP = withTopLevelBranchesP eqAndOrdP

filterPropertyValueP :: P (Filter PropertyValue)
filterPropertyValueP = withTopLevelBranchesP subP

pieceP :: Text -> P ()
pieceP t = void $ string' $ t <> ":"

maybeP :: P (Filter a) -> P (Filter (Maybe a))
maybeP = undefined

subEqOrdP ::
     (Validity a, Show a, Ord a, FilterArgument a, FilterSubString a, FilterOrd a) => P (Filter a)
subEqOrdP = try eqAndOrdP <|> subP

subP :: (Validity a, Show a, Ord a, FilterArgument a, FilterSubString a) => P (Filter a)
subP = validP $ FilterSub <$> argumentP

eqAndOrdP :: (Validity a, Show a, Ord a, FilterArgument a, FilterOrd a) => P (Filter a)
eqAndOrdP = ordP

ordP :: (Validity a, Show a, Ord a, FilterArgument a, FilterOrd a) => P (Filter a)
ordP =
  validP $ do
    o <- comparisonP
    void $ string' ":"
    a <- argumentP
    pure $ FilterOrd o a

comparisonP :: P Comparison
comparisonP =
  asum
    [ try $ string' "lt" >> pure LTC
    , try $ string' "le" >> pure LEC
    , try $ string' "eq" >> pure EQC
    , try $ string' "ge" >> pure GEC
    , string' "gt" >> pure GTC
    ]

argumentP :: (Validity a, FilterArgument a) => P a
argumentP = do
  s <- some (satisfy $ \c -> Char.isPrint c && not (Char.isSpace c) && c /= ')')
  validP $ either fail pure $ parseArgument $ T.pack s

validP :: Validity a => P a -> P a
validP parser = do
  a <- parser
  case prettyValidate a of
    Left err -> fail err
    Right a' -> pure a'

withTopLevelBranchesP :: P (Filter a) -> P (Filter a)
withTopLevelBranchesP parser = asum [try $ filterNotP parser, try $ filterBinRelP parser, parser]

filterNotP :: P (Filter a) -> P (Filter a)
filterNotP parser = do
  pieceP "not"
  FilterNot <$> parser

filterBinRelP :: forall a. P (Filter a) -> P (Filter a)
filterBinRelP parser = do
  void $ char '('
  f <- try filterOrP <|> filterAndP
  void $ char ')'
  pure f
  where
    filterOrP :: P (Filter a)
    filterOrP = do
      f1 <- parser
      void $ string' " or "
      f2 <- parser
      pure $ FilterOr f1 f2
    filterAndP :: P (Filter a)
    filterAndP = do
      f1 <- parser
      void $ string' " and "
      f2 <- parser
      pure $ FilterAnd f1 f2
-- filterAndP :: P Filter
-- filterAndP = do
--   f1 <- filterP
--   void $ string' " and "
--   f2 <- filterP
--   pure $ FilterAnd f1 f2
--   try filterHasTagP <|> try filterTodoStateP <|> try filterFileP <|> try filterLevelP <|>
--   try filterHeaderP <|>
--   try filterExactPropertyP <|>
--   try filterHasPropertyP <|>
--   try filterParentP <|>
--   try filterAncestorP <|>
--   try filterChildP <|>
--   try filterLegacyP <|>
--   try filterNotP <|>
--   filterBinRelP
--
-- filterHasTagP :: P Filter
-- filterHasTagP = do
--   void $ string' "tag:"
--   s <- many (satisfy $ \c -> Char.isPrint c && not (Char.isSpace c) && not (Char.isPunctuation c))
--   either fail (pure . FilterHasTag) $ parseTag $ T.pack s
--
-- filterTodoStateP :: P Filter
-- filterTodoStateP = do
--   void $ string' "state:"
--   s <- many (satisfy $ \c -> Char.isPrint c && not (Char.isSpace c) && not (Char.isPunctuation c))
--   either fail (pure . FilterTodoState) $ parseTodoState $ T.pack s
--
-- filterFileP :: P Filter
-- filterFileP = do
--   void $ string' "file:"
--   s <- many (satisfy $ \c -> not (Char.isSpace c) && c /= ')')
--   r <- either (fail . show) (pure . FilterFile) $ parseRelFile s
--   case prettyValidate r of
--     Left err -> fail err
--     Right f -> pure f
--
-- filterLevelP :: P Filter
-- filterLevelP = do
--   void $ string' "level:"
--   FilterLevel <$> decimal
--
-- filterHeaderP :: P Filter
-- filterHeaderP = do
--   void $ string' "header:"
--   s <- many (satisfy $ \c -> Char.isPrint c && not (Char.isSpace c) && c /= ')')
--   either fail (pure . FilterHeader) $ parseHeader $ T.pack s
--
-- filterParentP :: P Filter
-- filterParentP = do
--   void $ string' "parent:"
--   FilterParent <$> filterP
--
-- filterAncestorP :: P Filter
-- filterAncestorP = do
--   void $ string' "ancestor:"
--   FilterAncestor <$> filterP
--
-- filterChildP :: P Filter
-- filterChildP = do
--   void $ string' "child:"
--   FilterChild <$> filterP
--
-- filterLegacyP :: P Filter
-- filterLegacyP = do
--   void $ string' "legacy:"
--   FilterLegacy <$> filterP
--
-- filterHasPropertyP :: P Filter
-- filterHasPropertyP = do
--   void $ string' "has-property:"
--   FilterHasProperty <$> propertyNameP
--
-- filterExactPropertyP :: P Filter
-- filterExactPropertyP = do
--   void $ string' "exact-property:"
--   pn <- propertyNameP
--   void $ string' ":"
--   pv <- propertyValueP
--   pure $ FilterExactProperty pn pv
--
-- propertyNameP :: P PropertyName
-- propertyNameP = do
--   s <- many (satisfy $ \c -> Char.isPrint c && not (Char.isSpace c) && not (Char.isPunctuation c))
--   either fail pure $ parsePropertyName $ T.pack s
--
-- propertyValueP :: P PropertyValue
-- propertyValueP = do
--   s <- many (satisfy $ \c -> Char.isPrint c && not (Char.isSpace c) && not (Char.isPunctuation c))
--   either fail pure $ parsePropertyValue $ T.pack s
--
--
--
--
--
--
--
--
-- filterCompleter :: String -> [String]
-- filterCompleter = makeCompleterFromOptions ':' filterCompleterOptions
--
-- data CompleterOption
--   = Nullary String [String]
--   | Unary String
--   deriving (Show, Eq)
--
-- filterCompleterOptions :: [CompleterOption]
-- filterCompleterOptions =
--   [ Nullary "tag" ["out", "online", "offline", "toast", "personal", "work"]
--   , Nullary "state" ["CANCELLED", "DONE", "NEXT", "READY", "STARTED", "TODO", "WAITING"]
--   , Nullary "file" []
--   , Nullary "level" []
--   , Nullary "property" []
--   , Unary "parent"
--   , Unary "ancestor"
--   , Unary "child"
--   , Unary "legacy"
--   , Unary "not"
--   ]
--
-- makeCompleterFromOptions :: Char -> [CompleterOption] -> String -> [String]
-- makeCompleterFromOptions separator os s =
--   case separate separator (dropSeparatorAtEnd s) of
--     [] -> allOptions
--     pieces ->
--       let l = last pieces
--           prefix = intercalate [separator] pieces :: String
--           searchResults =
--             mapMaybe (\o -> (,) o <$> searchString l (renderCompletionOption o)) os :: [( CompleterOption
--                                                                                         , SearchResult)]
--        in flip concatMap searchResults $ \(o, sr) ->
--             case sr of
--               PrefixFound _ -> [renderCompletionOption o <> [separator]]
--               ExactFound ->
--                 case o of
--                   Unary _ -> map ((prefix <> [separator]) <>) allOptions
--                   Nullary _ rest -> map ((prefix <> [separator]) <>) rest
--   where
--     allOptions :: [String]
--     allOptions = map ((<> [separator]) . renderCompletionOption) os
--     dropSeparatorAtEnd :: String -> String
--     dropSeparatorAtEnd = reverse . dropWhile (== separator) . reverse
--
-- data SearchResult
--   = PrefixFound String
--   | ExactFound
--
-- searchString :: String -> String -> Maybe SearchResult
-- searchString needle haystack =
--   if needle `isPrefixOf` haystack
--     then Just $
--          if needle == haystack
--            then ExactFound
--            else PrefixFound needle
--     else Nothing
--
-- renderCompletionOption :: CompleterOption -> String
-- renderCompletionOption co =
--   case co of
--     Nullary s _ -> s
--     Unary s -> s
--
-- separate :: Char -> String -> [String]
-- separate c s =
--   case dropWhile (== c) s of
--     "" -> []
--     s' -> w : words s''
--       where (w, s'') = break (== c) s'
