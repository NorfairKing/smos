{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.FilterSpec
  ( spec
  ) where

import Data.Char as Char
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import Path

import Test.Hspec
import Test.QuickCheck as QC
import Test.Validity
import Test.Validity.Aeson

import Text.Megaparsec

import Cursor.Forest.Gen ()

import Smos.Data

import Cursor.Simple.Forest

import Smos.Report.Path.Gen ()

import Smos.Report.Filter
import Smos.Report.Filter.Gen ()
import Smos.Report.Path
import Smos.Report.Time hiding (P)

spec :: Spec
spec = do
  filterArgumentSpec @Time
  filterArgumentSpec @Tag
  filterArgumentSpec @Header
  filterArgumentSpec @TodoState
  filterArgumentSpec @PropertyName
  filterArgumentSpec @PropertyValue
  filterArgumentSpec @TimestampName
  filterArgumentSpec @Timestamp
  filterArgumentSpec @(Path Rel File)
  eqSpecOnValid @EntryFilter
  genValidSpec @EntryFilter
  eqSpecOnValid @(Filter RootedPath)
  genValidSpec @(Filter RootedPath)
  eqSpecOnValid @(Filter Time)
  genValidSpec @(Filter Time)
  eqSpecOnValid @(Filter Tag)
  genValidSpec @(Filter Tag)
  eqSpecOnValid @(Filter Header)
  genValidSpec @(Filter Header)
  eqSpecOnValid @(Filter TodoState)
  genValidSpec @(Filter TodoState)
  eqSpecOnValid @(Filter PropertyValue)
  genValidSpec @(Filter PropertyValue)
  -- jsonSpecOnValid @EntryFilter
  describe "foldFilterAnd" $
    it "produces valid results" $
    producesValidsOnValids (foldFilterAnd @(RootedPath, ForestCursor Entry))
  describe "filterPredicate" $
    it "produces valid results" $
    producesValidsOnValids2 (filterPredicate @(RootedPath, ForestCursor Entry))
  describe "argumentP" $ do
    parseJustSpec argumentP "." ("." :: Header)
    parseJustSpec argumentP "a" ("a" :: Header)
    parseJustSpec argumentP "\68339" ("\68339" :: Header)
  describe "subP" $ do
    parseJustSpec subP "." (FilterSub ("." :: Header))
    parseJustSpec subP "a" (FilterSub ("a" :: Header))
    parseJustSpec subP "\68339" (FilterSub ("\68339" :: Header))
  describe "filterRootedPathP" $ do
    parsesValidSpec filterRootedPathP filterRootedPathText
    renderFilterSpecFor filterRootedPathP
  describe "filterTimeP" $ do
    parseJustSpec filterTimeP "eq:15m" (FilterOrd EQC $ Minutes 15)
    parseJustSpec filterTimeP "lt:2h" (FilterOrd LTC $ Hours 2)
    parseJustSpec filterTimeP "le:8d" (FilterOrd LEC $ Days 8)
    parsesValidSpec filterTimeP filterTimeText
    renderFilterSpecFor filterTimeP
  describe "filterTagP" $ do
    parseJustSpec filterTagP "test" (FilterSub "test")
    parsesValidSpec filterTagP filterTagText
    renderFilterSpecFor filterTagP
  describe "filterHeaderP" $ do
    parseJustSpec filterHeaderP "\68339" (FilterSub "\68339")
    parseJustSpec filterHeaderP "test" (FilterSub "test")
    parsesValidSpec filterHeaderP filterHeaderText
    renderFilterSpecFor filterHeaderP
  describe "filterTodoStateP" $ do
    parseJustSpec filterTodoStateP "test" (FilterSub "test")
    parsesValidSpec filterTodoStateP filterTodoStateText
    renderFilterSpecFor filterTodoStateP
  -- describe "filterTimestampP" $ do
  --   parsesValidSpec filterTimestampP filterTimestampText
  --   renderFilterSpecFor filterTimestampP
  describe "filterPropertyValueP" $ do
    parseJustSpec filterPropertyValueP "test" (FilterSub "test")
    parsesValidSpec filterPropertyValueP filterPropertyValueText
    renderFilterSpecFor filterPropertyValueP
  describe "filterEntryP" $ do
    let p = parseJustSpec filterEntryP
    p "header:test" (FilterEntryHeader $ FilterSub "test")
    p "state:NEXT" (FilterEntryTodoState $ FilterMaybe False $ FilterSub "NEXT")
    p "property:timebox" (FilterEntryProperties $ FilterMapHas "timebox")
    p "properties:has:timebox" (FilterEntryProperties $ FilterMapHas "timebox")
    p
      "property:client:fpco"
      (FilterEntryProperties $ FilterMapVal "client" $ FilterMaybe False $ FilterSub "fpco")
    p
      "properties:client:fpco"
      (FilterEntryProperties $ FilterMapVal "client" $ FilterMaybe False $ FilterSub "fpco")
    p
      "property:timewindow:time:lt:5h"
      (FilterEntryProperties $
       FilterMapVal "timewindow" $
       FilterMaybe False $ FilterPropertyTime $ FilterMaybe False $ FilterOrd LTC $ Hours 5)
    parsesValidSpec filterEntryP filterEntryText
    renderFilterSpecFor filterEntryP
  -- describe "entryFilterP" $ do
  --   parsesValidSpec entryFilterP entryFilterText
  --   -- parseJustSpec filterP "tag:work" (FilterHasTag "work")
  --   -- parseJustSpec filterP "state:NEXT" (FilterTodoState "NEXT")
  --   -- parseJustSpec filterP "level:5" (FilterLevel 5)
  --   -- parseJustSpec filterP "exact-property:effort:30m" (FilterExactProperty "effort" "30m")
  --   -- parseJustSpec filterP "has-property:effort" (FilterHasProperty "effort")
  --   describe "renderFilter" $ do
  --     it "produces valid texts" $
  --       producesValidsOnValids (renderFilter @(RootedPath, ForestCursor Entry))
  --     it "renders filters that parse to the same" $
  --       forAllValid $ \f -> parseJust entryFilterP (renderFilter f) f

renderFilterSpecFor ::
     forall a. GenValid (Filter a)
  => P (Filter a)
  -> Spec
renderFilterSpecFor p =
  describe "renderFilter" $ do
    it "produces valid texts" $ producesValidsOnValids (renderFilter @a)
    it "renders filters that parse to the same" $ forAllValid $ \f -> parseJust p (renderFilter f) f

filterArgumentSpec ::
     forall a. (Show a, Eq a, GenValid a, FilterArgument a)
  => Spec
filterArgumentSpec =
  specify "parseArgument and renderArgument are inverses" $
  forAllValid $ \a -> parseArgument (renderArgument (a :: a)) `shouldBe` Right (a :: a)

entryFilterText :: Gen Text
entryFilterText = filterRootedPathText

filterRootedPathText :: Gen Text
filterRootedPathText = withTopLevelBranchesText $ textPieces [pieceText "file", genValid]

filterTimeText :: Gen Text
filterTimeText =
  withTopLevelBranchesText $
  eqAndOrdText $
  textPieces [T.pack . show <$> (genValid :: Gen Word), elements ["s", "m", "h", "d", "w"]]

filterTagText :: Gen Text
filterTagText = withTopLevelBranchesText $ subEqOrdText argumentText

filterHeaderText :: Gen Text
filterHeaderText = withTopLevelBranchesText $ subEqOrdText argumentText

filterTodoStateText :: Gen Text
filterTodoStateText = withTopLevelBranchesText $ subEqOrdText argumentText

filterTimestampText :: Gen Text
filterTimestampText = withTopLevelBranchesText $ eqAndOrdText argumentText

filterPropertyValueText :: Gen Text
filterPropertyValueText = withTopLevelBranchesText $ subEqOrdText argumentText

filterEntryText :: Gen Text
filterEntryText =
  withTopLevelBranchesText $
  oneof [filterHeaderText] -- TODO , maybeText filterTodoStateText, filterPropertiesText, filterTagsText]

subEqOrdText :: Gen Text -> Gen Text
subEqOrdText gen = oneof [eqAndOrdText gen, gen]

eqAndOrdText :: Gen Text -> Gen Text
eqAndOrdText gen = textPieces [oneof [pieceText "eq", pieceText "lt", pieceText "gt"], gen]

withTopLevelBranchesText :: Gen Text -> Gen Text
withTopLevelBranchesText gen = oneof [notText gen, binRelText gen, gen]

notText :: Gen Text -> Gen Text
notText gen = textPieces [pure "not:", gen]

binRelText :: Gen Text -> Gen Text
binRelText gen = textPieces [pure "(", oneof [orText, andText], pure ")"]
  where
    orText :: Gen Text
    orText = textPieces [gen, pure " or ", gen]
    andText :: Gen Text
    andText = textPieces [gen, pure " and ", gen]

pieceText :: Text -> Gen Text
pieceText t = pure $ t <> ":"

argumentText :: Gen Text
argumentText =
  T.pack <$> genListOf (genValid `suchThat` (\c -> Char.isPrint c && not (Char.isSpace c)))

--   describe "filterHasTagP" $ parsesValidSpec filterHasTagP tagText
--   describe "filterTodoStateP" $ parsesValidSpec filterTodoStateP todoStateText
--   describe "filterFileP" $ parsesValidSpec filterFileP fileText
--   describe "filterLevelP" $ parsesValidSpec filterLevelP levelText
--   describe "filterHeaderP" $ parsesValidSpec filterHeaderP headerText
--   describe "filterParentP" $ parsesValidSpec filterParentP parentText
--   describe "filterAncestorP" $ parsesValidSpec filterAncestorP ancestorText
--   describe "filterChildP" $ parsesValidSpec filterChildP childText
--   describe "filterLegacyP" $ parsesValidSpec filterLegacyP legacyText
--   describe "filterNotP" $ parsesValidSpec filterNotP notText
--   describe "filterBinrelP" $ parsesValidSpec filterBinRelP binRelText
--   describe "filterOrP" $ parsesValidSpec filterOrP orText
--   describe "filterAndP" $ parsesValidSpec filterAndP andText
--   describe "filterExactPropertyP" $ parsesValidSpec filterExactPropertyP exactPropertyText
--   describe "filterHasPropertyP" $ parsesValidSpec filterHasPropertyP hasPropertyText
--   describe "filterCompleter" $ do
--     let c s ss =
--           it (unwords ["completes", show s, "to", show ss]) $
--           sort (filterCompleter s) `shouldBe` sort ss
--         cp s ss = xdescribe "Not implemented yet." $ c s ss
--     c "t" ["tag:"]
--     c "s" ["state:"]
--     c "n" ["not:"]
--     c "p" ["parent:", "property:"]
--     c
--       "not"
--       [ "not:tag:"
--       , "not:state:"
--       , "not:file:"
--       , "not:level:"
--       , "not:property:"
--       , "not:parent:"
--       , "not:ancestor:"
--       , "not:child:"
--       , "not:legacy:"
--       , "not:not:"
--       ]
--     c
--       "not:"
--       [ "not:tag:"
--       , "not:state:"
--       , "not:file:"
--       , "not:level:"
--       , "not:property:"
--       , "not:parent:"
--       , "not:ancestor:"
--       , "not:child:"
--       , "not:legacy:"
--       , "not:not:"
--       ]
--     c "tag" ["tag:out", "tag:online", "tag:personal", "tag:offline", "tag:toast", "tag:work"]
--     c "tag:" ["tag:out", "tag:online", "tag:personal", "tag:offline", "tag:toast", "tag:work"]
--     cp "tag:h" ["tag:home"]
--     cp "state:N" ["state:NEXT"]
--
-- filterText :: Gen Text
-- filterText =
--   oneof
--     [ tagText
--     , todoStateText
--     , fileText
--     , levelText
--     , headerText
--     , hasPropertyText
--     , exactPropertyText
--     , parentText
--     , ancestorText
--     , childText
--     , legacyText
--     , notText
--     , binRelText
--     ]
--
-- tagText :: Gen Text
-- tagText =
--   textPieces
--     [ pure "tag:"
--     , T.pack <$>
--       genListOf
--         (genValid `suchThat`
--          (\c -> Char.isPrint c && not (Char.isSpace c) && not (Char.isPunctuation c)))
--     ]
--
-- todoStateText :: Gen Text
-- todoStateText =
--   textPieces
--     [ pure "state:"
--     , T.pack <$>
--       genListOf
--         (genValid `suchThat`
--          (\c -> Char.isPrint c && not (Char.isSpace c) && not (Char.isPunctuation c)))
--     ]
--
-- fileText :: Gen Text
-- fileText = textPieces [pure "file:", genValid]
--
-- levelText :: Gen Text
-- levelText = textPieces [pure "level:", T.pack . show <$> (genValid :: Gen Int)]
--
-- headerText :: Gen Text
-- headerText =
--   textPieces
--     [ pure "header:"
--     , T.pack <$>
--       genListOf
--         (genValid `suchThat`
--          (\c -> Char.isPrint c && not (Char.isSpace c) && not (Char.isPunctuation c)))
--     ]
--
-- parentText :: Gen Text
-- parentText = textPieces [pure "parent:", filterText]
--
-- ancestorText :: Gen Text
-- ancestorText = textPieces [pure "ancestor:", filterText]
--
-- childText :: Gen Text
-- childText = textPieces [pure "child:", filterText]
--
-- legacyText :: Gen Text
-- legacyText = textPieces [pure "legacy:", filterText]
--
--
-- exactPropertyText :: Gen Text
-- exactPropertyText =
--   textPieces [pure "exact-property:", propertyNameText, pure ":", propertyValueText]
--
-- hasPropertyText :: Gen Text
-- hasPropertyText = textPieces [pure "has-property:", propertyNameText]
--
-- -- These don't match exactly, but they're a good start.
-- propertyNameText :: Gen Text
-- propertyNameText =
--   T.pack <$>
--   genListOf
--     (genValid `suchThat`
--      (\c -> Char.isPrint c && not (Char.isSpace c) && not (Char.isPunctuation c)))
--
-- -- These don't match exactly, but they're a good start.
-- propertyValueText :: Gen Text
-- propertyValueText =
--   T.pack <$>
--   genListOf
--     (genValid `suchThat`
--      (\c -> Char.isPrint c && not (Char.isSpace c) && not (Char.isPunctuation c)))
--
textPieces :: [Gen Text] -> Gen Text
textPieces = fmap T.concat . sequenceA

parseJustSpec :: (Show a, Eq a) => P a -> Text -> a -> Spec
parseJustSpec p s res = it (unwords ["parses", show s, "as", show res]) $ parseJust p s res

parsesValidSpec :: (Show a, Eq a, Validity a) => P a -> Gen Text -> Spec
parsesValidSpec p gen = it "only parses valid values" $ forAll gen $ parsesValid p

parseJust :: (Show a, Eq a) => P a -> Text -> a -> Expectation
parseJust p s res =
  case parse (p <* eof) "test input" s of
    Left err ->
      expectationFailure $
      unlines ["P failed on input", show s, "with error", errorBundlePretty err]
    Right out -> out `shouldBe` res

parsesValid :: (Show a, Eq a, Validity a) => P a -> Text -> Property
parsesValid p s =
  checkCoverage $
  let (useful, ass) =
        case parse (p <* eof) "test input" s of
          Left _ -> (False, pure () :: IO ())
          Right out -> (True, shouldBeValid out)
   in cover 10.0 useful "useful" $ property ass
