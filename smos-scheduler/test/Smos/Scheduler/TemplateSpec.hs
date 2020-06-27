{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Scheduler.TemplateSpec
  ( spec,
  )
where

import Data.GenValidity
import Data.GenValidity.Text ()
import Smos.Scheduler.Template
import Test.Hspec
import Test.Validity

instance GenValid Template where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid TemplatePiece where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

spec :: Spec
spec = do
  genValidSpec @TemplatePiece
  genValidSpec @Template
  describe "renderTemplatePiece" $ it "produces valids values" $ producesValidsOnValids renderTemplatePiece
  describe "renderTemplate" $ it "produces valids values" $ producesValidsOnValids renderTemplate
  describe "renderTemplate and parseTemplate" $ it "are inverses" $ forAllValid $ \t -> parseTemplate (renderTemplate t) `shouldBe` Right t
  describe "parseTemplate" $ do
    it "parses into valid values" $ producesValidsOnValids parseTemplate
    let s t ps =
          let r = Template ps
           in it ("succesfully parses " <> show t <> " into " <> show r) $ parseTemplate t `shouldBe` Right r
    let f t = it ("correctly fails to parse " <> show t) $ case parseTemplate t of
          Left _ -> pure ()
          Right r -> expectationFailure $ "Should have failed to parse, but succeeded and parsed " <> show r
    s "" []
    s "hello" [TLit "hello"]
    s "hello world" [TLit "hello world"]
    s "hello[world]" [TLit "hello", TTime "world"]
    s "hello[ world ]" [TLit "hello", TTime "world"]
    s "[]" [TTime ""]
    f "hello["
    f "hello[world"
    f "hello[ world"
