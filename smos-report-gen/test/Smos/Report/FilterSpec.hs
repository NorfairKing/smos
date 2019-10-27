{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.FilterSpec
  ( spec
  ) where

import Debug.Trace

import Data.Char as Char
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import Path

import Test.Hspec
import Test.QuickCheck as QC
import Test.Validity
import Test.Validity.Aeson

import Control.Monad

import Text.Megaparsec

import Cursor.Forest.Gen ()
import Cursor.Simple.Forest

import Smos.Data

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
  -- describe "optimiseFilter" $ do
  --   it "produces valid results" $
  --     producesValidsOnValids (optimiseFilter @(RootedPath, ForestCursor Entry))
  --   it "produces the filters that produce the same result" $
  --     forAllValid $ \f ->
  --       let f' = optimiseFilter (f :: EntryFilter)
  --        in forAllValid $ \a -> filterPredicate f' a `shouldBe` filterPredicate f a
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
  describe "filterEntryHeaderP" $ do
    let p = parseJustSpec filterEntryHeaderP
    p "header:test" (FilterEntryHeader $ FilterSub "test")
    parsesValidSpec filterEntryHeaderP filterEntryHeaderText
  describe "filterEntryTodoStateP" $ do
    let p = parseJustSpec filterEntryTodoStateP
    p "state:NEXT" (FilterEntryTodoState $ FilterMaybe False $ FilterSub "NEXT")
    p
      "state:(NEXT or READY)"
      (FilterEntryTodoState $ FilterMaybe False $ FilterOr (FilterSub "NEXT") (FilterSub "READY"))
    p
      "state:(STARTED or (NEXT or READY))"
      (FilterEntryTodoState $
       FilterMaybe False $
       FilterOr (FilterSub "STARTED") $ FilterOr (FilterSub "NEXT") (FilterSub "READY"))
    parsesValidSpec filterEntryTodoStateP filterEntryTodoStateText
  describe "filterEntryPropertiesP" $ do
    let p = parseJustSpec filterEntryPropertiesP
    p "property:timebox" (FilterEntryProperties $ FilterMapHas "timebox")
    p
      "property:client:fpco"
      (FilterEntryProperties $ FilterMapVal "client" $ FilterMaybe False $ FilterSub "fpco")
    p
      "property:timewindow:time:lt:5h"
      (FilterEntryProperties $
       FilterMapVal "timewindow" $
       FilterMaybe False $ FilterPropertyTime $ FilterMaybe False $ FilterOrd LTC $ Hours 5)
    parsesValidSpec filterEntryPropertiesP filterEntryPropertiesText
  describe "filterEntryTagsP" $ do
    let p = parseJustSpec filterEntryTagsP
    p "tag:work" (FilterEntryTags $ FilterAny $ FilterSub "work")
    p "tag:any:online" (FilterEntryTags $ FilterAny $ FilterSub "online")
    parsesValidSpec filterEntryTagsP filterEntryTagsText
  describe "filterEntryP" $ do
    let p = parseJustSpec filterEntryP
    p "header:test" (FilterEntryHeader $ FilterSub "test")
    p "state:NEXT" (FilterEntryTodoState $ FilterMaybe False $ FilterSub "NEXT")
    p
      "state:(NEXT or READY)"
      (FilterEntryTodoState $ FilterMaybe False $ FilterOr (FilterSub "NEXT") (FilterSub "READY"))
    p
      "state:(STARTED or (NEXT or READY))"
      (FilterEntryTodoState $
       FilterMaybe False $
       FilterOr (FilterSub "STARTED") $ FilterOr (FilterSub "NEXT") (FilterSub "READY"))
    p "tag:work" (FilterEntryTags $ FilterAny $ FilterSub "work")
    p "tag:any:online" (FilterEntryTags $ FilterAny $ FilterSub "online")
    p "property:timebox" (FilterEntryProperties $ FilterMapHas "timebox")
    p
      "property:client:fpco"
      (FilterEntryProperties $ FilterMapVal "client" $ FilterMaybe False $ FilterSub "fpco")
    p
      "property:timewindow:time:lt:5h"
      (FilterEntryProperties $
       FilterMapVal "timewindow" $
       FilterMaybe False $ FilterPropertyTime $ FilterMaybe False $ FilterOrd LTC $ Hours 5)
    parsesValidSpec filterEntryP filterEntryText
    -- renderFilterSpecFor filterEntryP DOESN'T ACTUALLY HOLD
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
    it "renders filters that parse to the same" $
      forAllValid $ \f -> do
        let t = renderFilter f
        case parse (p <* eof) "test input" t of
          Left err ->
            expectationFailure $
            unlines ["P failed on input", show t, "with error", errorBundlePretty err]
          Right f' ->
            unless (f == f') $
            expectationFailure $
            unlines
              [ "Rendered and parsed the value, but parsed to a different value"
              , "expected:"
              , show f
              , "actual:"
              , show f'
              , "rendered text:"
              , T.unpack t
              ]

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

filterEntryHeaderText :: Gen Text
filterEntryHeaderText = textPieces [pieceText "header", filterHeaderText]

filterEntryTodoStateText :: Gen Text
filterEntryTodoStateText = textPieces [pieceText "state", maybeText filterTodoStateText]

filterEntryPropertiesText :: Gen Text
filterEntryPropertiesText = textPieces [pieceText "property", mapText filterPropertyValueText]

filterEntryTagsText :: Gen Text
filterEntryTagsText = textPieces [pieceText "tag", listText filterTagText]

filterEntryText :: Gen Text
filterEntryText =
  withTopLevelBranchesText $
  oneof
    [ filterEntryHeaderText
    , filterEntryTodoStateText
    , filterEntryPropertiesText
    , filterEntryTagsText
    ]

subEqOrdText :: Gen Text -> Gen Text
subEqOrdText gen = oneof [eqAndOrdText gen, gen]

eqAndOrdText :: Gen Text -> Gen Text
eqAndOrdText gen =
  textPieces
    [oneof [pieceText "lt", pieceText "le", pieceText "eq", pieceText "ge", pieceText "gt"], gen]

withTopLevelBranchesText :: Gen Text -> Gen Text
withTopLevelBranchesText gen = oneof [notText gen, binRelText gen, gen]

mapText :: Gen Text -> Gen Text
mapText gen =
  oneof
    [ withTopLevelBranchesText $ textPieces [pieceText "val", gen]
    , withTopLevelBranchesText $ textPieces [pieceText "has", gen]
    , withTopLevelBranchesText $ textPieces [argumentText >>= pieceText, gen]
    , gen
    ]

listText :: Gen Text -> Gen Text
listText gen =
  oneof
    [ withTopLevelBranchesText $ textPieces [pieceText "any", gen]
    , withTopLevelBranchesText $ textPieces [pieceText "all", gen]
    , gen
    ]

maybeText :: Gen Text -> Gen Text
maybeText gen =
  oneof
    [ withTopLevelBranchesText $ textPieces [pieceText "maybe-true", gen]
    , withTopLevelBranchesText $ textPieces [pieceText "maybe-false", gen]
    , gen
    ]

notText :: Gen Text -> Gen Text
notText gen = textPieces [pieceText "not", gen]

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

parseJustSpec :: (Validity a, Show a, Eq a) => P a -> Text -> a -> Spec
parseJustSpec p s res = it (unwords ["parses", show s, "as", show res]) $ parseJust p s res

parsesValidSpec :: (Show a, Eq a, Validity a) => P a -> Gen Text -> Spec
parsesValidSpec p gen = it "only parses valid values" $ forAll gen $ parsesValid p

parseJust :: (Validity a, Show a, Eq a) => P a -> Text -> a -> Expectation
parseJust p s res =
  case parse (p <* eof) "test input" s of
    Left err ->
      expectationFailure $
      unlines ["P failed on input", show s, "with error", errorBundlePretty err]
    Right out -> do
      shouldBeValid res
      shouldBeValid out
      out `shouldBe` res

parsesValid :: (Show a, Eq a, Validity a) => P a -> Text -> Property
parsesValid p s =
  checkCoverage $
  let (useful, ass) =
        case parse (p <* eof) "test input" s of
          Left _ -> (False, pure () :: IO ())
          Right out -> (True, shouldBeValid out)
   in cover 10.0 useful "useful" $ property ass
