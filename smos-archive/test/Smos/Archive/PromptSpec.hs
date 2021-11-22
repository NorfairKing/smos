{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Archive.PromptSpec
  ( spec,
  )
where

import Smos.Archive.Prompt
import Test.Syd
import Test.Syd.Validity

instance GenValid YesNo

spec :: Spec
spec = do
  genValidSpec @YesNo
  describe "yesNoPromptString" $
    it "produces valid strings" $
      producesValid yesNoPromptString
  describe "parseYesNo" $ it "produces valid YesNo's" $ producesValid parseYesNo
