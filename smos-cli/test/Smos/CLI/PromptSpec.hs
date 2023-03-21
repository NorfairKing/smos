{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.CLI.PromptSpec (spec) where

import Data.GenValidity.Text ()
import Smos.CLI.Prompt
import Test.Syd
import Test.Syd.Validity

instance GenValid YesNo

spec :: Spec
spec = do
  genValidSpec @YesNo
  describe "yesNoPromptText" $
    it "produces valid strings" $
      producesValid yesNoPromptText
  describe "parseYesNo" $ it "produces valid YesNo's" $ producesValid parseYesNo
