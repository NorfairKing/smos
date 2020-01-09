{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Archive.PromptSpec
  ( spec
  ) where

import Test.Hspec
import Test.Validity

import Smos.Archive.Prompt

instance GenUnchecked YesNo

instance GenValid YesNo

spec :: Spec
spec = do
  genValidSpec @YesNo
  describe "yesNoPromptString" $
    it "produces valid strings" $ producesValidsOnValids yesNoPromptString
  describe "parseYesNo" $ it "produces valid YesNo's" $ producesValidsOnValids parseYesNo
