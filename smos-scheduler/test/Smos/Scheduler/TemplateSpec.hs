{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Scheduler.TemplateSpec
  ( spec,
  )
where

import Data.GenValidity.Text ()
import Smos.Scheduler.Template
import Smos.Scheduler.Template.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @TemplatePiece
  genValidSpec @Template
  describe "normaliseTemplate" $ it "produces valid templates" $ producesValidsOnValids normaliseTemplate
  describe "renderTimeTemplatePiece" $ it "produces valids values" $ producesValidsOnValids renderTimeTemplatePiece
  describe "renderTimeTemplate" $ it "produces valids values" $ producesValidsOnValids renderTimeTemplate
  describe "renderTimeTemplate and parseTimeTemplate" $ it "are inverses" $ forAllValid $ \t -> parseTimeTemplate (renderTimeTemplate t) `shouldBe` Right t
  describe "parseTemplate" $ do
    it "parses into valid values" $ producesValidsOnValids parseTimeTemplate
    let s t ps =
          let r = Template ps
           in it ("succesfully parses " <> show t <> " into " <> show r) $ parseTimeTemplate t `shouldBe` Right r
    let f t = it ("correctly fails to parse " <> show t) $ case parseTimeTemplate t of
          Left _ -> pure ()
          Right r -> expectationFailure $ "Should have failed to parse, but succeeded and parsed " <> show r
    -- Literal
    s "" []
    s "hello" [TLit "hello"]
    s "hello world" [TLit "hello world"]
    -- Time
    s "[]" [TTime ""]
    s "hello[world]" [TLit "hello", TTime "world"]
    s "hello[ world ]" [TLit "hello", TTime "world"]
    f "hello["
    f "hello[world"
    f "hello[ world"
    -- Relative Time
    s "[|]" [TRelTime "" ""]
    s "foo[bar|quux]" [TLit "foo", TRelTime "bar" "quux"]
    s "foo[ bar | quux ]" [TLit "foo", TRelTime "bar" "quux"]
