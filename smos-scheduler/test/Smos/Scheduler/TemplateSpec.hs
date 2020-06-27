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
  describe "parseTemplate" $ it "parses into valid values" $ producesValidsOnValids parseTemplate
  describe "renderTemplatePiece" $ it "produces valids values" $ producesValidsOnValids renderTemplatePiece
  describe "renderTemplate" $ it "produces valids values" $ producesValidsOnValids renderTemplate
  describe "renderTemplate and parseTemplate" $ it "are inverses" $ forAllValid $ \t -> parseTemplate (renderTemplate t) `shouldBe` Right t
